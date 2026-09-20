# CLAUDE.md

This file provides guidance to [Claude Code](https://claude.com/product/claude-code) when working with code in this repository.
It is not intended for human eyes.

### Maintenance

You (robot or human) have standing permission to update this file without asking.
Add important patterns, gotchas, or context that would help future sessions.
Keep it concise and actionable.

## Project overview

Kairos: a scheduling tool that picks the best (venue, date) option for an event by
eliciting as little availability information from participants as possible. See
[README.md](README.md) for the problem statement, the measured behaviour, and the
formal background.

### Where things live

- `common` holds the whole domain model *and* the whole solver, cross-compiled to JVM
  and JS. Put anything with real logic here; the server and client should be thin.
- The solver pipeline is Belief -> Ensemble -> Forecast/Verdict -> Elicitation, with
  `Analysis.of` as the single entry point.
- `server` serves both the JSON API (`/api/polls/...`) and the three HTML pages, which
  all use the same template and differ only in which view name is injected.
- `client` reads the poll and participant identifiers out of `window.location` via
  `views/Route.scala`, since the template does not pass them.

### Things that will bite you

- **Do not score questions by expected improvement in the choice.** It is exactly zero
  whenever a single answer would not flip which option leads, which is most of the
  time, so rounds stall after a few questions. `Elicitation` uses mutual information
  with the identity of the winning option instead. The full reasoning is in that
  file's Scaladoc; read it before changing the objective.
- **Question worth is in bits and is not monotonically decreasing** along a round,
  because mutual information is not submodular within a participant. Do not "fix" this.
- **The value of information can rise between rounds.** Also not a bug.
- `Service` deliberately provides only `api`. Documentation and metrics are assembled
  once across all services in `Assembly`; a service that provided its own would
  collide with the next one (Prometheus refuses duplicate metric registration, and
  only the first `/docs` would be reachable).
- `Day` is a hand-rolled epoch-day type because `java.time` has no Scala.js build.
  `DaySuite` checks it against known dates over a 160-year span.
- Tapir schemas are written out explicitly in `api/Schemas.scala` rather than derived
  with `sttp.tapir.generic.auto`, which fails opaquely on the opaque types and enums.
- The forked dev JVM's working directory is `server/`, so the data file path is set
  explicitly via the `data.file` system property in `Subprojects.scala`.

### Running it

- `sbt dev` then [localhost:8080](http://localhost:8080); click "Create a worked example".
- `sbt "common/testOnly -- *"` runs everything, including the end-to-end simulation.
  Plain `sbt test` only reruns changed suites under sbt 2.

### Deployment

See [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md). `.github/workflows/deploy.yml` builds the `Dockerfile`
after CI passes on `main`, pushes it to GHCR and runs `deploy/compose.yml` (the app behind Caddy) on
the server named by the `DEPLOY_HOST` variable over SSH; pull requests touching packaging only build
and try the image. The fat JAR does not contain its assets: the image copies the server's
`resource_managed/main/assets` beside it and passes `-Dassets.dir`. Serve the site from a host of
its own, never a sub-path, as every URL is absolute. Anything the server reads from its environment
is supplied through the `APP_ENV` secret, never baked into the image.

## Instructions

### Compilation and Diagnostics

- When the user asks for help with a compilation or type error, start by running `sbt compile` to see the error for yourself.
  If there are many errors, making it unclear which one the user is referring to, ask them to clarify, and then focus only on that issue.
- IntelliJ MCP integration is active. When a request seems to implicitly refer to something the user is looking at, always check
  `mcp__ide__getDiagnostics` first to see which file(s) are open and get associated diagnostics (errors, warnings, and info hints with line numbers).

#### Testing

- After making code changes, always run `sbt compile` to verify that issues are fixed and no new ones are introduced.
- Repeatedly retry upon failure until the build succeeds. If you are unsure how to fix an issue, ask for help or refer to existing code for examples.
- Before trying to fix an error, make sure you first understand it fully.
- You should never report that a feature is complete without testing it first.

### Code Style

- You must read the [Code Style Guidelines](docs/STYLE_GUIDE.md).

### Pull Requests

When asked to publish the code changes, your task is to open one or more pull requests (PRs) to merge the changes into `main` on GitHub:

- Use `git` to check what has changed as compared to the `main` branch on `origin`.
- If the changes are thematically linked, they can be published as a single PR.
- Otherwise, you'll need to divide the changes into multiple PRs using your own judgement.
- Each PR should have a singular focus, shouldn't break anything, and should be able to be merged independently.
- Ensure that all code is staged, committed and pushed. Ensure no new files are left uncommitted, and no debug code is left in the codebase.
- When creating a PR, ensure that the title and description are clear, informative, and comprehensive.
- All feature/bugfix/etc branch names should be formatted as "feature_<short description>" or "fix_<short description>" or similar.
- All PR titles should be formatted as "[<scope>] <Short summary>", e.g. "[renderer] Fixed colour inversion bug."
- You have GitHub MCP integration that can be used to do the above.
