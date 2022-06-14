
#' Line-wraps an `rlang` non-`cli`-format condition message
#'
#' `rlang` condition messages can be character vectors with names indicating
#' line prefixes such as bullet points. There is a basic format and a `cli`
#' format. The basic format does not undergo line wrapping, so this function can
#' be used to apply line wrapping ahead of time; it is incompatible with
#' `use_cli_format=TRUE`(or would at least require `wrap_prefix=""`), as `cli`
#' does its own paragraph formatting that will join across newlines and rewrap.
#'
#' @param message possibly-named character vector; an `rlang`-style,
#'   non-`cli`-format condition message
#' @param width `width` argument for [`base::strwrap`]
#' @param first_part_external_prefix content that we expect to be printed by
#'   `rlang` before the first message part (if `message` has at least one part),
#'   such as `"! "`
#' @param later_parts_external_prefix content of the same width of the content
#'   that we expect to be printed by `rlang` before every part other than the
#'   first, e.g., fancy versions of `"* "`, `"i "`, `"x "`, or two spaces
#' @param first_part_wrap_prefix prefix to add to new lines that are introduced
#'   by line wrapping the first part of the message (if `message` has at least
#'   one part)
#' @param later_parts_wrap_prefix prefix to add to , e.g., two spaces to align
#'   with the text in bullet points
#'
#' @return named character vector; an `rlang`-style, non-`cli`-format condition
#'   message
#'
#' @importFrom purrr map_chr
#' @importFrom rlang names2
#' @importFrom stats setNames
wrap_noncli_message = function(message, width = getOption("width")-1L,
                               first_part_external_prefix = "! ", later_parts_external_prefix = "* ",
                               first_part_wrap_prefix = later_parts_wrap_prefix, later_parts_wrap_prefix = "  ") {
  if (length(message) == 0L) {
    return (character(0L))
  } else {
    prefixed_message_parts_lines = setNames(
      c(strwrap(message[[1L]], width, initial = first_part_external_prefix, prefix = first_part_wrap_prefix, simplify=FALSE),
        strwrap(message[-1L], width, initial = later_parts_external_prefix, prefix = later_parts_wrap_prefix, simplify=FALSE)),
      names2(message)
    )
    prefixed_message_parts = map_chr(prefixed_message_parts_lines, paste, collapse="\n")
    nchar_in_prefixes = c(nchar(first_part_external_prefix),
                          rep(nchar(later_parts_external_prefix), length(prefixed_message_parts)-1L))
    message_parts <- substring(prefixed_message_parts, nchar_in_prefixes + 1L)
    return(message_parts)
  }
}

env_pkg_name_global_hit_text = "Hit global environment while looking for package namespace among env and ancestors.  This indicates `env` was not associated with a package, e.g., it was from `source`d-in scripts or typed at the interactive prompt.  (It might also indicate that `env_pkg_name`'s caller was trying to grab an execution environment from somewhere on the call stack and jumped the wrong number of stack frames.)  This error message can be replaced with a return value using the `global_hit_result` argument."

env_pkg_name_empty_hit_text = "Hit empty environment while looking for package namespace among env and ancestors"

env_pkg_name_imports_hit_text = 'Hit package imports/"imports:" environment while looking for package namespace among env or ancestors; maybe the input `env` was the result of at least one too many `parent.env` calls?'

env_pkg_name_exports_hit_text = 'Hit package exports/package/"package:" environment while looking for package *namespace* among `env` or ancestors, and before hitting the global environment; maybe after the package was attached, something then constructed `env` as a descendant of the package exports environment?'

#' Get the name of the package that calls this, or of another env or call
#'
#' Specifically, check `env` and its ancestors until finding a package namespace
#' environment, and return the name of that package. By default, `env` is the
#' calling environment, so this function can be used to get the name of a
#' package from inside one of its functions. If during the search, the global
#' environment, empty environment, or a package imports environment is
#' encountered, then the search has failed, as a package namespace environment
#' should not be an ancestor of any of those environments; by default, output an
#' error based on the first of these environments encountered.
#'
#' @param env optional; a function execution environment, a function's enclosure
#'   environment, or other environment; by default, the function execution
#'   environment from which this function was called (the caller's execution
#'   environment) or the global environment, if it was not called from another
#'   function, so that, if this function is called with no args from a package
#'   function, it will give its package's name, and if it is called from a
#'   global function, will give the default `global_hit_result`
#' @param global_hit_result optional; non-function or a `function(input_env,
#'   current_env) <result>`; what result to return when there appears to be no
#'   associated package, and the global environment is hit during the ancestor
#'   search. This is the expected result when this function is called from code
#'   outside of a package (but it may also happen if the caller grabbed `env`
#'   from the call stack, and jumped the wrong number of stack frames). The
#'   default is to raise an error in this case, but pass in a string here and it
#'   will be returned instead.
#' @param empty_hit_result optional; non-function or a `function(input_env,
#'   current_env) <result>`; what result to return when there appears to be no
#'   associated package, and the empty environment is hit during the ancestor
#'   search. This is expected to happen if called on an environment default is
#'   to produce an error.
#' @param imports_hit_result optional; non-function or a `function(input_env,
#'   current_env) <result>`; what result to return when there appears to be no
#'   associated package, and a package imports environment is hit during the
#'   ancestor search. This case is expected to occur only in advanced use when
#'   there's been some issue with requesting the wrong ancestor of an
#'   environment when preparing the `env` argument. Default is to produce an
#'   error.
#' @param exports_hit_result optional; non-function or a `function(input_env,
#'   current_env) <result>`; what result to return when there appears to be no
#'   associated package, and a package exports environment is hit during the
#'   ancestor search. This is not expected to occur in any foreseen use case,
#'   and might only be possible if, after the package is attached, `env` is
#'   manually constructed as a descendant of the package exports environment.
#'   Default is to produce an error.
#' @param namespace_hit_result optional; non-function or a `function(input_env,
#'   current_env) <result>`; what result to return when we successfully find a
#'   package namespace environment among `env` or its ancestors;
#'
#' @return a string containing the associated package name, or one of the
#'   overrides specified in {`global_hit_result`, `empty_hit_result`,
#'   `imports_hit_result`, `exports_hit_result`, `namespace_hit_result`}
#'
#' @details The tests to detect a package imports and exports environments are
#'   mostly name-based and thus may be a little imprecise, but also may be more
#'   robust to any package-related environment restructuring that may happen (or
#'   may have happened already) in other versions of R.
#'
#' @importFrom rlang caller_env is_reference global_env base_env empty_env
#'   is_function
#' @importFrom tibble lst
env_pkg_name = function(env=caller_env(),
                        global_hit_result =
                          function(input_env, current_env)
                            Abort(env_pkg_name_global_hit_text,
                                  "env_pkg_name__global_hit",
                                  lst(input_env, current_env)),
                        empty_hit_result =
                          function(input_env, current_env)
                            Abort(env_pkg_name_empty_hit_text,
                                  "env_pkg_name__empty_hit",
                                  lst(input_env, current_env)),
                        imports_hit_result =
                          function(input_env, current_env)
                            Abort(env_pkg_name_imports_hit_text,
                                  "env_pkg_name__imports_hit",
                                  lst(input_env, current_env)),
                        exports_hit_result =
                          function(input_env, current_env)
                            Abort(env_pkg_name_exports_hit_text,
                                  "env_pkg_name__exports_hit",
                                  lst(input_env, current_env)),
                        namespace_hit_result =
                          function(input_env, current_env)
                            sub("^namespace:", "", environmentName(current_env))
                        ) {
  if (!is.environment(env)) {
    Abort("`env` must be an environment",
          "env_pkg_name__env_must_be_environment",
          lst(env))
  }
  to_result_fn = function(result) {
    if (is_function(result)) {
      result
    } else {
      function(input_env, current_env) { result }
    }
  }
  global_hit_result_fn = to_result_fn(global_hit_result)
  empty_hit_result_fn = to_result_fn(empty_hit_result)
  imports_hit_result_fn = to_result_fn(imports_hit_result)
  exports_hit_result_fn = to_result_fn(exports_hit_result)
  namespace_hit_result_fn = to_result_fn(namespace_hit_result)
  # Reminder of what `parent.env`-following would look like for the output `f`
  # of a function factory `factory` in package `<pkgname>`:
  #   `environment(f)` = factory execution env
  #   -> `environment(factory)` = package's namespace env ("namespace:<pkgname>")
  #   -> package imports env ("imports:<pkgname>")
  #   -> base namespace env ("namespace:base")
  #   -> global env
  #   -> ...
  #   -> package's package/exports env (typically "package:<pkgname>")
  #   -> ...
  #   -> base package/exports env ("base")
  #   -> empty env
  input_env = env
  current_env = env
  while (TRUE) {
    # Typical advice is to use `identical` to compare environments for
    # ((variations of) "exact") equality, but `help(identical)` doesn't spell
    # out exactly what this means for environments. From a little testing, it
    # seems (using defaults) to mean referential equality. This seems
    # reasonable, assuming there's nothing exotic (env cloning, etc.) going on
    # with `global_env`(=`globalenv`). So try referential equality, but
    # explicitly, using `rlang::is_reference`.
    if (is_reference(current_env, global_env())) {
      # This result will likely be encountered when trying out this function in
      # an interactive session, as it seems common to try out on things defined
      # in the global environment rather than actually in a package.
      return (global_hit_result_fn(input_env, current_env))
    } else if (is_reference(current_env, empty_env())) {
      # This is probably a user error.
      return (empty_hit_result_fn(input_env, current_env))
    } else {
      env_nm = environmentName(current_env)
      # This name-based test is a little inexact. We could further check that
      # the parent environment is the base namespace environment, but would
      # need to make sure that this isn't version or system dependent. It
      # doesn't seem that likely that an environment would be accidentally
      # named something starting with "imports:" anyway.
      if (grepl("^imports:", env_nm)) {
        # This is probably a user error.
        return (imports_hit_result_fn(input_env, current_env))
      } else if (grepl("^package:", env_nm) || is_reference(current_env, base_env())) {
        # Note that the base "exports" `environmentName` doesn't have the
        # "package:" prefix like other "exports" environments.
        # (`rlang::env_name` makes it consistent, but also repeats many of the
        # checks already done.)
        #
        # This is probably a user error.
        return (exports_hit_result_fn(input_env, current_env))
      } else if(isNamespace(current_env)) {
        # Successfully found a package namespace env ("namespace:<pkgname>"),
        # the package's internal operating area. (We should hit this case under
        # normal circumstances when used by some error-related or utility
        # function to figure out the package of *its* caller.)
        return (namespace_hit_result_fn(input_env, current_env))
      } else {
        current_env <- parent.env(current_env) # (and continue looping)
      }
    }
  }
}

#' Condition handler calling [`utils::recover`], with a consistent identity
#'
#' @template handler_fn
#'
#' @section Side effects: runs [`utils::recover()`]
#' @return output of [`utils::recover`] (likely/always `NULL`)
#'
#' @family condition/calling handler functions
#'
#' @export
handle_via_recover = `environment<-`(function(cnd) utils::recover(),
                                     baseenv())

#' Condition handler muffling warnings, with a consistent identity
#'
#' @template handler_fn
#'
#' @section Side effects: muffles a warning (invokes the `"muffleWarning"`
#'   restart)
#' @return output of [`base::invokeRestart`] (likely/always `NULL`)
#'
#' @family condition/calling handler functions
#'
#' @export
handle_via_muffle_warning = `environment<-`(function(wrn) invokeRestart("muffleWarning"),
                                            baseenv())

#' Condition handler muffling messages, with a consistent identity
#'
#' @template handler_fn
#'
#' @section Side effects: muffles a message (invokes the `"muffleMessage"`
#'   restart)
#' @return output of [`base::invokeRestart`] (likely/always `NULL`)
#'
#' @family condition/calling handler functions
#'
#' @export
handle_via_muffle_message = `environment<-`(function(msg) invokeRestart("muffleMessage"),
                                         baseenv())

#' Condition handler calling [`utils::dump.frames`], with a consistent identity
#'
#' @template handler_fn
#' @param cnd a condition object (e.g., an error, warning, or message)
#'
#' @section Side effects: runs [`utils::dump.frames()`]
#' @return output of [`utils::dump.frames`] (likely/always `NULL`)
#'
#' @family condition/calling handler functions
#'
#' @export
handle_via_dump_frames = `environment<-`(function(cnd) utils::dump.frames(),
                                      baseenv())

#' Condition handler calling [`rlang::entrace`], with a consistent identity (same as `rlang:::hnd_entrace`)
#'
#' @template handler_fn
#'
#' @param cnd a condition object (e.g., an error, warning, or message)
#'
#' @section Side effects: runs [`rlang::entrace(cnd)`][rlang::entrace]
#'
#' @return output of [`rlang::entrace`] (likely/always `NULL`)
#'
#' @details This ends up being a re-implementation of `rlang:::hnd_entrace`, and
#'   so [`global_handlers_push`] and [`global_handlers_remove`] will work
#'   together with [`rlang::global_entrace`]. (The `rlang` implementation
#'   comments note additional difficulties maintaining a consistent identity,
#'   patched in \url{https://bugs.r-project.org/show_bug.cgi?id=18197}.)
#'
#' @family condition/calling handler functions
#'
#' @export
handle_via_entrace = `environment<-`(function(cnd) rlang::entrace(cnd),
                                  baseenv())

# Below and in related @template definitions, the pattern
#
# `r if(getRversion()>="4")paste0("[")` `base::.....` `r if(getRversion()>="4")paste0("]")`
#
# uses dynamic R code to conditionally turn `base::.....` into a link only if
# documenting on R version >=4. This is to prevent invalid links and package
# check issues when referencing something that only exists in R versions >=4
# when building the package on versions <4. If updating things based on this
# pattern, make sure the <4-built docs and >=4-built docs will make sense for
# both <4 and >=4 users.

#' Raise an error if `r if(getRversion()>="4")paste0("[")` `base::globalCallingHandlers` `r if(getRversion()>="4")paste0("]")` isn't supported
#'
#' @family functions working with global calling handler stack
check_global_handlers_stack_support = function() {
  # note that `getRversion()` outputs a special class; the below is not
  # straight string or numeric comparison
  if (getRversion() < "4") {
    Abort("Global calling handlers stack isn't supported until R version 4.0.",
          "check_global_handlers_stack__r_version_too_low",
          list(version=getRversion()))
  }
}

#' List current named global calling handlers `r if(getRversion()>="4")paste0("[")` `base::globalCallingHandlers()` `r if(getRversion()>="4")paste0("]")`
#'
#' This is just a version check followed by [`base::globalCallingHandlers`].
#' It's here just to make it easier for a user to find if they are first
#' introduced to global handlers via other functions in this package.
#'
#' @family functions working with global calling handler stack
#'
#' @export
global_handlers_list = function() {
  check_global_handlers_stack_support()
  # workaround for checks when built on earlier R versions: rather than
  # directly use `globalCallingHandlers`, grab it in a way that package checks
  # don't detect as problematic
  globalCallingHandlers = baseenv()[["globalCallingHandlers"]]
  globalCallingHandlers()
}

#' Get the max number of args a function can accept (as a double)
#'
#' @param f the function to inspect
#'
#' @return a double; if `f` takes in dots arguments (`...`), then `Inf`; else,
#'   the number of arguments that `f` accepts, converted from an integer to a
#'   double
#'
#' @importFrom rlang is_function abort dbl
fn_max_n_args = function(f) {
  if (!is_function(f)) {
    Abort("`f` is not a function",
          "fn_max_n_args__f_must_be_function",
          list(f=f))
  }
  # We need `args` here to work properly on primitive functions
  arg_names = names(formals(args(f)))
  if ("..." %in% arg_names) {
    Inf
  } else {
    dbl(length(arg_names))
  }
}

#' Raise an error if dots don't look like named condition handlers
#'
#' @template handler_dots
#'
#' @importFrom rlang names2 is_function
#' @importFrom tibble lst
#' @importFrom purrr map_lgl map_dbl
check_named_handlers_dots = function(...) {
  dots = list(...)
  dots_names = names2(dots)==""
  dots_are_functions = map_lgl(dots, is_function)
  dots_can_take_at_least_one_arg = map_dbl(dots, fn_max_n_args) >= 1
  if (any(dots_names) ||
      !all(dots_are_functions) ||
      # this isn't a complete check that the handler could be called on a
      # single argument without issues, but should catch niladic functions
      # (e.g., directly `recover`):
      !all(dots_can_take_at_least_one_arg)) {
    Abort("Arguments in `...` must all be nontrivially named, and must all be functions accepting at least one argument (the error/warning/message/other condition object), e.g., `error=handle_via_recover`",
          "check_named_handlers_dots",
          lst(dots_names, dots_are_functions, dots_can_take_at_least_one_arg),
          lst(dots))
  }
}

#' Remove each specified global calling handler (if established)
#'
#' @template handler_dots
#'
#' @template handler_fn_env_test
#'
#' @section Side effects: removes any named global calling handlers that were
#'   established
#'
#' @return a logical vector of length `length(list(...))` indicating which named
#'   handlers were established and removed
#'
#' @family functions working with global calling handler stack
#'
#' @importFrom rlang env_label fn_env arg_match
#' @importFrom tibble lst
#' @importFrom purrr map2_lgl
#' @export
global_handlers_remove = function(...,
                                  .fn_env_test=
                                    # any updates that change this default arg
                                    # should also be mirrored in functions that
                                    # duplicate this default (e.g.,
                                    # `global_handlers_push`):
                                    c("label", "identical", "ignore")
                                  ) {
  check_global_handlers_stack_support()
  check_named_handlers_dots(...)
  .fn_env_test <- arg_match(.fn_env_test)
  #
  unwanted_handlers = list(...)
  # workaround for checks when built on earlier R versions: rather than
  # directly use `globalCallingHandlers`, grab it in a way that package checks
  # don't detect as problematic
  globalCallingHandlers = baseenv()[["globalCallingHandlers"]]
  current_handlers = environment(globalCallingHandlers)[["gh"]]
  if (!is.list(current_handlers)) {
    Abort('Expected `environment(globalCallingHandlers)[["gh"]]` to be a list.',
          "global_handlers_remove__unexpected_gh",
          lst(env_label(environment(globalCallingHandlers)),
              environment(globalCallingHandlers)[["gh"]]),
          .internal=TRUE)
  }
  two_functions_equivalent =
    switch(.fn_env_test,
           # (All of these ignore bytecode and srcref.)
           "label" = function(x, y) {
             # Use `fn_env` rather than `environment` in order to work on
             # primitives + add checks. Use `env_label` rather than
             # `environmentName` so that anonymous environments with different
             # addresses are considered to be different.
             identical(x, y, ignore.environment=TRUE) &&
               env_label(fn_env(x)) == env_label(fn_env(y))
           },
           "identical" = identical,
           "ignore" = function(x, y) {
             identical(x, y, ignore.environment=TRUE)
           })
  map_two_functions_equivalent = function(x, y) map2_lgl(x, y, two_functions_equivalent)
  equivalence_matrix =
    outer(names(unwanted_handlers), names(current_handlers), `==`) &
    outer(unwanted_handlers, current_handlers, map_two_functions_equivalent)
  unwanted_handler_was_found_and_should_be_removed = rowSums(equivalence_matrix) > 0L
  current_handler_should_be_removed = colSums(equivalence_matrix) > 0L
  environment(globalCallingHandlers)[["gh"]] <- current_handlers[!current_handler_should_be_removed]
  # At this point, all removals should have been successful. Indicate which
  # unwanted handlers were actually found and removed:
  return (unwanted_handler_was_found_and_should_be_removed)
}

#' Push each specified handler onto/to top of global caller handler stack
#'
#' @template handler_dots
#' @template handler_fn_env_test
#'
#' @section Side effects: push each named handler onto/to the top of the global
#'   calling handler stack. If a specified named handler already exists on the
#'   stack (according to a function equality test informed by `.fn_env_test`),
#'   it is removed from its current position and pushed to the top of the stack;
#'   otherwise, it is added to the top of the stack; either way, the named
#'   handler ends up at the top of the stack, with no duplicate underneath. If
#'   there are multiple named handlers in `...`, the last one is the one that
#'   will ultimately end up on top of the global handler stack (and be the first
#'   to be consulted when a condition is triggered).
#'
#' @family functions working with global calling handler stack
#'
#' @export
global_handlers_push = function(..., .fn_env_test=c("label", "identical", "ignore")) {
  # some checks here are duplicated in `global_handlers_remove`, but
  # hopefully will be less surprising to see coming from here than there
  check_global_handlers_stack_support()
  check_named_handlers_dots(...)
  # globalCallingHandlers doesn't have the concept of .fn_env_test, so to
  # push without duplicates using .fn_env_test, remove any that count as
  # duplicates under .fn_env_test before adding all the handlers. This has the
  # same reordering behavior on duplicates as `globalCallingHandlers`, ordering
  # them (along with the other handlers being pushed) to be called first.
  # (With multiple handlers in `...`, the order within them will be the reverse
  # of `list(...)`.)
  global_handlers_remove(...)
  # workaround for checks when built on earlier R versions: rather than
  # directly use `globalCallingHandlers`, grab it in a way that package checks
  # don't detect as problematic
  globalCallingHandlers = baseenv()[["globalCallingHandlers"]]
  # `globalCallingHandlers` returns the handler list with no args; for
  # consistent behavior, skip this call if told to push 0 handlers (maybe
  # something is programmatically pushing handlers for this situation to come
  # up)
  if (length(list(...)) != 0L) globalCallingHandlers(...)
}

# TODO abort -> Abort, add class name, debug info

#' @importFrom rlang caller_env
rlang_condition_tutorial_message = function(maybe_pkg_name) {
  c(
    sprintf('Some operation triggered an error, warning, message, or other "condition".  Note that %s `rlang` conditions; these are generally compatible with ordinary conditions, but have some extra features.',
            if (is.null(maybe_pkg_name)) {
              "`Abort`, `Warn`, and `Inform` use"
            } else {
              paste0(maybe_pkg_name," uses")
            }),
    "*" = "Enhancement: use `rlang::last_trace()` or `rlang::last_error()$trace` to get a different style of traceback for the last `rlang` error; these `rlang` traces can help clarify, for example, when a problematic argument is passed into a function, but R's lazy evaluation mechanics delay its evaluation until the function (or one of its callees, their callees, etc.) rely on it.  If an error in a try-catch type of structure triggered the actual error raised, info about the underlying error may be available at `rlang::last_error()$parent` (and might already be displayed as part of an `rlang`-style trace).  The call triggering the error may be recorded in `rlang::last_error()$call`.",
    "*" = "Compatibility: try `global_handlers_push(error=handle_via_recover)` and `global_handlers_remove(error=handle_via_recover)` if `options(error=recover)` and `options(error=NULL)` does not work.",
    "*" = "Enhancement: use `rlang::global_entrace()` to enable `rlang` condition object saving for most errors (`rlang::last_error()`), warnings (`rlang::last_warnings()`), and messages (`rlang::last_messages()`).  These globally-entraced conditions have many or all of the features of native `rlang_error`s.",
    "*" = 'Enhancement: it is easy upstream to assign additional classes.  This package uses helper functions to generate condition objects with classes `c(["<pkgname>__<optional_condition_specific_class>",] "<pkgname>__<type>", "<pkgname>__condition", "rlang_<type>", "<type>", "condition")`, where `<type>` is one of `error`, `warning`, or `condition`. Any of these classes can be selectively caught and handled using `withCallingHandlers({<code>}, <class>={entrace,recover,dump_frames,muffle_warning,muffle_message}_handler)` for a single block of code, or with `global_handlers_push(<class>={entrace,recover,dump_frames}_handler)` and `global_handlers_remove(<class>={entrace,recover,dump_frames}_handler)` for blocks of code.  This allows, for example, calling `recover()` on every `<pkgname>__warning` encountered, resuming execution after inspection with `recover()` is finished. It also allows easy muffling of particular warnings or messages if they have been assigned their own class; use `rlang::global_entrace()` and check `lapply(rlang::last_{warnings,messages}(), class)`.',
    "*" = 'Enhancement: it is easy upstream to include debug info (metadata fields) in condition objects.  This package uses helper functions to generate condition objects `cnd` such that additional debug info is at `cnd$<pkgname>`',
    "*" = "Note: other debugging tools are still available, such as editor-placed breakpoints, `[un]debug[once](fn)`, `setBreakpoint`, `[un]trac{e,ingState}`, etc."
  )
}

#' @importFrom rlang caller_env
inform_rlang_condition_tutorial = function(pkg_name, .frequency="once") {
  Inform(
    rlang_condition_tutorial_message(pkg_name),
    class_suffix = "rlang_condition_tutorial",
    .frequency_id = paste0(pkg_name,"__","message_rlang_condition_tutorial"),
    .frequency = .frequency,
    show_tutorial = FALSE
  )
}

#' `rlang` non-`cli`-format condition `message` arg from potentially-long text and subfields
#'
#' @importFrom utils capture.output str
#' @importFrom rlang syms
condition_noncli_message = function(init, base_message, display_subfields, more_subfields, more_intro, more_var_name, no_more_intro=more_intro) {
  display_subfields_message_part =
    if (length(display_subfields) == 0L) {
      character(0L)
    } else {
      # TODO pointer to err$epiprocess
      #
      # (We won't wrap this part later to avoid wrapping the `str` output, so
      # keep the header short or `strwrap` it here)
      c("i" = paste0("Debug info:", paste(collapse="\n", capture.output(str(display_subfields)))))
    }
  more_subfields_message_part =
    if (length(more_subfields) == 0L) {
      c("i" = sprintf("%s.", no_more_intro))
    } else {
      # FIXME this is abort-specific
      # TODO .frequency inform
      c("i" = sprintf(
        "%s and `%s$epiprocess$%s%s%s`.",
        more_intro,
        more_var_name,
        if (length(more_subfields) > 1L) "{" else "",
        toString(syms(names(more_subfields))),
        if (length(more_subfields) > 1L) "}" else ""
      ))
    }
  return (c(
    wrap_noncli_message(base_message,
                        first_part_external_prefix = init,
                        # If we have a very short `init` up to 2 characters,
                        # just indent to align with the first line; otherwise,
                        # indent by two spaces to align with any bullet point
                        # text that follows
                        first_part_wrap_prefix = strrep(" ", min(2L, nchar(init)))),
    display_subfields_message_part,
    wrap_noncli_message(more_subfields_message_part)
  ))
}


#' Augmented version of [`rlang::abort`]
#'
#' @template Condition
#'
#' @importFrom rlang abort inject !!! chr
#' @importFrom stats setNames
Abort = function(message, class_suffix = NULL, display_subfields = list(), more_subfields = list(), call = caller_env(), show_tutorial = TRUE, ...) {
  maybe_pkg_name = env_pkg_name(call, global_hit_result = NULL)
  if (show_tutorial) {
    inform_rlang_condition_tutorial(maybe_pkg_name)
  }
  class_prefix = if (is.null(maybe_pkg_name)) {
                   ""
                 } else {
                   paste0(maybe_pkg_name,"__")
                 }
  specific_class = if (is.null(class_suffix)) {
                     chr()
                   } else {
                     paste0(class_prefix,class_suffix)
                   }
  all_added_classes = if (is.null(maybe_pkg_name)) {
                  specific_class
                } else {
                  chr(specific_class, paste0(class_prefix,chr("error", "condition")))
                }
  debug_info_field_name = if (is.null(maybe_pkg_name)) {
                            "debug_subfields"
                          } else {
                            maybe_pkg_name
                          }
  # rlang errors start with init "! "
  inject(abort(
    condition_noncli_message("! ", message, display_subfields, more_subfields,
                             "More info is available via `err=rlang::last_error(); print(err)`", "err",
                             # Just saying to print the error object for more if
                             # we don't have more subfields or other particulars
                             # to point to would be repetitive given `rlang`'s
                             # built-in instructions, so just don't say anything
                             # in this case (for now):
                             no_more_intro=character(0L)),
    use_cli_format = FALSE,
    class = all_added_classes,
    # `!!!lst(!!debug_info_field_name := ...)` would be more compact but is not
    # used here to avoid potential conflicts between `rlang`'s and
    # `data.table`'s `:=`s, here and in `Warn`, `Inform`:
    !!!setNames(list(c(display_subfields, more_subfields)), debug_info_field_name),
    call = call,
    ...
  ))
}

#' Augmented version of [`rlang::warn`]
#'
#' @template Condition
#'
#' @importFrom rlang warn inject !!!
#' @importFrom stats setNames
Warn = function(message, class_suffix = NULL, display_subfields = list(), more_subfields = list(), call = caller_env(), show_tutorial = TRUE, ...) {
  maybe_pkg_name = env_pkg_name(call, global_hit_result = NULL)
  if (show_tutorial) {
    inform_rlang_condition_tutorial(maybe_pkg_name)
  }
  class_prefix = if (is.null(maybe_pkg_name)) {
                   ""
                 } else {
                   paste0(maybe_pkg_name,"__")
                 }
  specific_class = if (is.null(class_suffix)) {
                     chr()
                   } else {
                     paste0(class_prefix,class_suffix)
                   }
  all_added_classes = if (is.null(maybe_pkg_name)) {
                  specific_class
                } else {
                  chr(specific_class, paste0(class_prefix,chr("warning", "condition")))
                }
  debug_info_field_name = if (is.null(maybe_pkg_name)) {
                            "debug_subfields"
                          } else {
                            maybe_pkg_name
                          }
  warn_option = getOption("warn", 0L)
  init = if (warn_option >= 2L) {
    # Either "! (converted from warning) " or "Error: (converted from warning)",
    # depending on whether warnings are being `rlang::entrace`d. It's
    # troublesome/hard to check this robustly, so instead just assume the longer
    # one to avoid missed wraps in the longer-prefix case, while settling for some
    # slightly early wraps in the shorter-prefix case.
    "Error: (converted from warning)"
  } else if (warn_option == 1L) {
    "Warning: "
  } else {
    # R prints "Warning message:" on a separate line, then the warning message
    # with a prefix of either "" or, if there were multiple warnings, "<warning
    # number>: <msg>"; as above, wrap based on the longest possible prefix:
    paste0(strrep("i", nchar(getOption("nwarnings"))),": ")
  }
  inject(warn(
    condition_noncli_message(init, message, display_subfields, more_subfields,
                             "If `rlang::global_entrace` is active, more info is available via `wrn=rlang::last_warnings()[[i]]; print(wrn)`", "wrn"),
    use_cli_format = FALSE,
    class = all_added_classes,
    # `!!!lst(!!debug_info_field_name := ...)` would be more compact but is not
    # used here to avoid potential conflicts between `rlang`'s and
    # `data.table`'s `:=`s, here and in `Warn`, `Inform`:
    !!!setNames(list(c(display_subfields, more_subfields)), debug_info_field_name),
    call = call,
    ...
  ))
}

#' Augmented version of [`rlang::inform`]
#'
#' @template Condition
#'
#' @importFrom rlang inform inject !!!
#' @importFrom stats setNames
Inform = function(message, class_suffix = NULL, display_subfields = list(), more_subfields = list(), call = caller_env(), show_tutorial = TRUE, ...) {
  maybe_pkg_name = env_pkg_name(call, global_hit_result = NULL)
  if (show_tutorial) {
    inform_rlang_condition_tutorial(maybe_pkg_name)
  }
  class_prefix = if (is.null(maybe_pkg_name)) {
                   ""
                 } else {
                   paste0(maybe_pkg_name,"__")
                 }
  specific_class = if (is.null(class_suffix)) {
                     chr()
                   } else {
                     paste0(class_prefix,class_suffix)
                   }
  all_added_classes = if (is.null(maybe_pkg_name)) {
                  specific_class
                } else {
                  chr(specific_class, paste0(class_prefix,chr("message", "condition")))
                }
  debug_info_field_name = if (is.null(maybe_pkg_name)) {
                            "debug_subfields"
                          } else {
                            maybe_pkg_name
                          }
  # rlang messages start with init ""
  inject(inform(
    condition_noncli_message("", message, display_subfields, more_subfields,
                             character(0L), "msg"),
    use_cli_format = FALSE,
    class = all_added_classes,
    # `!!!lst(!!debug_info_field_name := ...)` would be more compact but is not
    # used here to avoid potential conflicts between `rlang`'s and
    # `data.table`'s `:=`s, here and in `Warn`, `Inform`:
    !!!setNames(list(c(display_subfields, more_subfields)), debug_info_field_name),
    call = call,
    ...
  ))
}
