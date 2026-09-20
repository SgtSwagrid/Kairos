package com.alecdorrington.server
package store

import cats.effect.{IO, Ref}
import cats.effect.std.Mutex
import cats.syntax.all.*
import com.alecdorrington.common.model.{Id, Poll}
import io.circe.parser.decode
import io.circe.syntax.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant

/**
  * The polls this server knows about, held in memory and mirrored to a file so
  * that a restart does not lose a poll half-way through its rounds.
  *
  * Writing the whole collection out on every change is wasteful, and entirely
  * adequate: a poll is amended a few dozen times over several weeks by a
  * handful of people, so there is nothing to gain from anything cleverer and a
  * good deal of complexity to avoid.
  *
  * @param state
  *   Every poll, in the order they were stored.
  *
  * @param file
  *   Where to mirror the polls, or [[None]] to keep them in memory alone.
  *
  * @param writing
  *   Held for the duration of a save, so that two cannot interleave.
  */
final class Polls private (
  state: Ref[IO, Vector[Poll]],
  file: Option[Path],
  writing: Mutex[IO],
):

  /** Every poll, most recently created first. */
  def all: IO[List[Poll]] = state.get.map(_.reverse.toList)

  /** The poll of the given identifier, if there is one. */
  def get(id: Id[Poll]): IO[Option[Poll]] = state.get.map(_.find(_.id == id))

  /**
    * Stores a poll, replacing any existing poll of the same identifier.
    *
    * @param poll
    *   The poll to store.
    *
    * @return
    *   The poll as stored.
    */
  def put(poll: Poll): IO[Poll] = state.update(polls =>
    polls.indexWhere(_.id == poll.id) match
      case -1    => polls :+ poll
      case found => polls.updated(found, poll),
  ) *> save.as(poll)

  /**
    * Amends the poll of the given identifier, if there is one.
    *
    * The amendment runs inside the update, so it is retried against the latest
    * polls should anything else have changed them meanwhile. It must therefore
    * be cheap, and must touch only what it means to change: an amendment that
    * rebuilt a whole poll from a copy read earlier would undo whatever had
    * arrived while it was being prepared.
    *
    * @param id
    *   The identifier of the poll to amend.
    *
    * @param amend
    *   How to amend it.
    *
    * @return
    *   The amended poll, or [[None]] if there is no such poll.
    */
  def update(id: Id[Poll])(amend: Poll => Poll): IO[Option[Poll]] = state
    .modify: polls =>
      polls.indexWhere(_.id == id) match
        case -1    => (polls, None)
        case found =>
          val amended = amend(polls(found))
          (polls.updated(found, amended), Some(amended))
    .flatTap(amended => save.whenA(amended.isDefined))

  /**
    * Deletes the poll of the given identifier.
    *
    * @param id
    *   The identifier of the poll to delete.
    *
    * @return
    *   Whether there was such a poll to delete.
    */
  def discard(id: Id[Poll]): IO[Boolean] = state
    .modify(polls => (polls.filterNot(_.id == id), polls.exists(_.id == id)))
    .flatTap(removed => save.whenA(removed))

  /**
    * Writes every poll out to [[file]].
    *
    * Saves are serialised, and each writes to a scratch file of its own before
    * replacing the destination. Sharing one scratch path was enough to lose
    * data: two guests answering at once is the ordinary case, and one save
    * could truncate the scratch file while another was part-way through moving
    * it into place, leaving a fragment of JSON behind.
    *
    * A failure is reported and swallowed rather than failing the request, since
    * the poll is safely in memory either way and the participant has no use for
    * the news.
    */
  private def save: IO[Unit] = file.traverse_ : destination =>
    writing
      .lock
      .surround:
        state
          .get
          .flatMap: polls =>
            IO.blocking:
                Option(destination.getParent).foreach(Files.createDirectories(
                  _,
                ))
                val scratch = Files.createTempFile(
                  destination.toAbsolutePath.getParent,
                  destination.getFileName.toString,
                  ".tmp",
                )
                Files.write(
                  scratch,
                  polls.toList.asJson.spaces2.getBytes(UTF_8),
                )
                Files.move(
                  scratch,
                  destination,
                  StandardCopyOption.REPLACE_EXISTING,
                )
              .void
      .handleErrorWith(error =>
        IO.println(s"Could not save polls to $destination: ${ error
            .getMessage }"),
      )

object Polls:

  /**
    * Opens the store, reading back whatever was last written.
    *
    * @param file
    *   Where the polls are mirrored, or [[None]] to keep them in memory alone.
    *
    * @return
    *   The store, populated from `file`.
    */
  def open(file: Option[Path]): IO[Polls] =
    for
      loaded <- file
        .filter(Files.exists(_))
        .traverse(read)
        .map(_.getOrElse((Nil, true)))
      (restored, mirrorable) = loaded
      state   <- Ref.of[IO, Vector[Poll]](restored.toVector)
      writing <- Mutex[IO]
    yield new Polls(
      state,
      Option.when(mirrorable)(file).flatten,
      writing,
    )

  /**
    * Reads the polls held in the given file.
    *
    * A file that cannot be read is set aside under a new name before the server
    * carries on without it. Merely ignoring it was worse than it sounds: the
    * store would start empty and the very next answer would save over the file,
    * turning a passing failure — a backup or a virus scanner holding it open
    * for a moment — into the permanent loss of every poll.
    *
    * @param file
    *   The file to read.
    *
    * @return
    *   The polls it held, and whether it is safe to save over.
    */
  private def read(file: Path): IO[(List[Poll], Boolean)] = IO
    .blocking(String(Files.readAllBytes(file), UTF_8))
    .map(decode[List[Poll]](_))
    .flatMap:
      case Right(polls) =>
        IO.println(s"Restored ${ polls.size } polls.").as((polls, true))
      case Left(error) => setAside(file, error.getMessage)
    .handleErrorWith(error => setAside(file, error.getMessage))

  /**
    * Moves an unusable file out of the way, so that it is not saved over.
    *
    * @param file
    *   The file that could not be read.
    *
    * @param reason
    *   Why it could not be read, for the log.
    *
    * @return
    *   No polls, and whether it is now safe to save over the file.
    */
  private def setAside(file: Path, reason: String): IO[(List[Poll], Boolean)] =
    val kept = file.resolveSibling(
      s"${ file.getFileName }.${ Instant.now.toEpochMilli }.bad",
    )
    IO.blocking(Files.move(file, kept))
      .attempt
      .flatMap: moved =>
        IO.println(
          s"Could not read $file ($reason). " + moved.fold(
              failure =>
                s"Nor could it be set aside (${ failure.getMessage }), so " +
                  "polls will be kept in memory only until it is dealt with.",
              _ => s"It has been kept as $kept.",
            ),
        ) *> IO.pure((Nil, moved.isRight))
