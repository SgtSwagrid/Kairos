package com.alecdorrington.server
package store

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.common.model.{Id, Poll}
import io.circe.parser.decode
import io.circe.syntax.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}

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
  *   Every poll, indexed by identifier.
  *
  * @param file
  *   Where to mirror the polls, or [[None]] to keep them in memory alone.
  */
final class Polls private (
  state: Ref[IO, Map[Id[Poll], Poll]],
  file: Option[Path],
):

  /** Every poll, most recently created first. */
  def all: IO[List[Poll]] = state.get.map(_.values.toList.reverse)

  /** The poll of the given identifier, if there is one. */
  def get(id: Id[Poll]): IO[Option[Poll]] = state.get.map(_.get(id))

  /**
    * Stores a poll, replacing any existing poll of the same identifier.
    *
    * @param poll
    *   The poll to store.
    *
    * @return
    *   The poll as stored.
    */
  def put(poll: Poll): IO[Poll] = state.update(_.updated(poll.id, poll)) *>
    save *> poll.pure

  /**
    * Amends the poll of the given identifier, if there is one.
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
      polls.get(id).map(amend) match
        case None          => (polls, None)
        case Some(amended) => (polls.updated(id, amended), Some(amended))
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
    .modify(polls => (polls - id, polls.contains(id)))
    .flatTap(removed => save.whenA(removed))

  /** Writes every poll out to [[file]], atomically where the platform allows. */
  private def save: IO[Unit] = file.traverse_ : destination =>
    state
      .get
      .flatMap: polls =>
        IO.blocking:
            val scratch =
              destination.resolveSibling(s"${ destination.getFileName }.tmp")
            Option(destination.getParent).foreach(Files.createDirectories(_))
            Files.write(
              scratch,
              polls.values.toList.asJson.spaces2.getBytes(UTF_8),
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
    * A file that cannot be read is reported and then ignored rather than
    * treated as fatal, so that a corrupt or outdated file does not leave the
    * server unable to start.
    *
    * @param file
    *   Where the polls are mirrored, or [[None]] to keep them in memory alone.
    *
    * @return
    *   The store, populated from `file`.
    */
  def open(file: Option[Path]): IO[Polls] =
    for
      restored <- file
        .filter(Files.exists(_))
        .traverse(read)
        .map(_.getOrElse(Nil))
      state <- Ref.of[IO, Map[Id[Poll], Poll]](
        restored.map(poll => poll.id -> poll).toMap,
      )
    yield new Polls(state, file)

  /**
    * Reads the polls held in the given file, yielding none if it cannot be
    * read.
    */
  private def read(file: Path): IO[List[Poll]] = IO
    .blocking(String(Files.readAllBytes(file), UTF_8))
    .map(decode[List[Poll]](_))
    .flatMap:
      case Right(polls) =>
        IO.println(s"Restored ${ polls.size } polls.").as(polls)
      case Left(error) =>
        IO.println(s"Ignoring unreadable $file: ${ error.getMessage }").as(Nil)
    .handleErrorWith(error =>
      IO.println(s"Could not read $file: ${ error.getMessage }").as(Nil),
    )
