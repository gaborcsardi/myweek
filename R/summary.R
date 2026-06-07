repos_created <- function(evts) {
  rps <- keep(
    evts,
    \(x) x$type == "CreateEvent" && x$payload$ref_type == "repository"
  )
  add_class(
    sort(map_chr(rps, c("repo", "name"))),
    "repos_created"
  )
}

forks_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "ForkEvent")
  fc <- data.frame(
    repo = map_chr(rps, c("payload", "forkee", "full_name")),
    upstream = map_chr(rps, c("repo", "name"))
  )
  add_class(fc, "forks_created")
}

tags_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "CreateEvent" && x$payload$ref_type == "tag")
  tc <- data.frame(
    repo = map_chr(rps, c("repo", "name")),
    tag = map_chr(rps, c("payload", "ref"))
  )
  tc <- tc[order(tc$repo, tc$tag), ]
  add_class(tc, "tags_created")
}

branches_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "CreateEvent" && x$payload$ref_type == "branch")
  bc <- data.frame(
    repo = map_chr(rps, c("repo", "name")),
    branch = map_chr(rps, c("payload", "ref"))
  )
  bc <- bc[order(bc$repo, bc$branch), ]
  add_class(bc, "branches_created")
}

issues_opened <- function(evts) {
  iss <- keep(evts, \(x) x$type == "IssuesEvent" &&
    x$payload$action %in% c("opened", "reopened"))
  io <- data.frame(
    repo = map_chr(iss, c("repo", "name")),
    number = map_int(iss, c("payload", "issue", "number")),
    title = map_chr(iss, c("payload", "issue", "title"))
  )
  add_class(io, "issues_opened")
}

issues_closed <- function(evts) {
  iss <- keep(evts, \(x) x$type == "IssuesEvent" &&
    x$payload$action %in% c("closed"))
  io <- data.frame(
    repo = map_chr(iss, c("repo", "name")),
    number = map_int(iss, c("payload", "issue", "number")),
    title = map_chr(iss, c("payload", "issue", "title"))
  )
  add_class(io, "issues_closed")
}

commits_pushed <- function(evts) {
  pss <- keep(evts, \(x) x$type == "PushEvent" && length(x$payload$commits) > 0)
  cp <- list_rbind(map(pss, function(ev) {
    cmts <- rev(ev$payload$commits)
    data.frame(
      repo = ev$repo$name,
      ref = ev$payload$ref,
      message = first_line(map_chr(cmts, "message")),
      sha = map_chr(cmts, "sha")
    )
  }))
  if (nrow(cp) > 0) {
    cp <- cp[order(cp$repo), ]
  }
  add_class(cp, "commits_pushed")
}

summarize_events <- function(evts) {
  list(
    repos_created = repos_created(evts),
    forks_created = forks_created(evts),
    tags_created = tags_created(evts),
    branches_created = branches_created(evts),
    issues_opened = issues_opened(evts),
    issues_closed = issues_closed(evts),
    commits_pushed = commits_pushed(evts)
  )
}
