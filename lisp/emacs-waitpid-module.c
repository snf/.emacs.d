#include <emacs-module.h>

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

int plugin_is_GPL_compatible;

typedef emacs_value (*waitpid_module_function) (emacs_env *env, ptrdiff_t nargs,
                                                emacs_value *args, void *data);

static emacs_value
waitpid_nil(emacs_env *env)
{
  return env->intern(env, "nil");
}

static emacs_value
waitpid_list(emacs_env *env, ptrdiff_t nargs, emacs_value *args)
{
  return env->funcall(env, env->intern(env, "list"), nargs, args);
}

static emacs_value
waitpid_signal_error(emacs_env *env, const char *message)
{
  emacs_value args[] = { env->make_string(env, message, (ptrdiff_t) strlen(message)) };
  env->non_local_exit_signal(env, env->intern(env, "error"),
                             waitpid_list(env, 1, args));
  return waitpid_nil(env);
}

static emacs_value
waitpid_plist(emacs_env *env, pid_t pid, int status, int err)
{
  const char *error_text = err == 0 ? "" : strerror(err);
  emacs_value args[] = {
    env->intern(env, ":pid"), env->make_integer(env, (intmax_t) pid),
    env->intern(env, ":status"), env->make_integer(env, (intmax_t) status),
    env->intern(env, ":errno"), env->make_integer(env, (intmax_t) err),
    env->intern(env, ":error"),
    env->make_string(env, error_text, (ptrdiff_t) strlen(error_text))
  };

  return waitpid_list(env, 8, args);
}

static emacs_value
Fwaitpid(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) nargs;
  (void) data;

  pid_t pid = (pid_t) env->extract_integer(env, args[0]);
  int options = (int) env->extract_integer(env, args[1]);
  int status = 0;

  errno = 0;
  pid_t result = waitpid(pid, &status, options);
  int err = result < 0 ? errno : 0;

  return waitpid_plist(env, result, status, err);
}

static emacs_value
Fwaitpid_reap_loop(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) data;

  intmax_t limit = 2000;
  if (nargs > 0 && env->is_not_nil(env, args[0]))
    limit = env->extract_integer(env, args[0]);

  if (limit < 0)
    return waitpid_signal_error(env, "waitpid reap limit must be non-negative");

  intmax_t count = 0;
  while (count < limit) {
    int status = 0;
    errno = 0;
    pid_t result = waitpid((pid_t) -1, &status, WNOHANG);

    if (result > 0) {
      count++;
    } else if (result == 0 || errno == ECHILD) {
      return env->make_integer(env, count);
    } else {
      return waitpid_signal_error(env, strerror(errno));
    }
  }

  return env->make_integer(env, count);
}

static void
waitpid_defun(emacs_env *env, const char *name, ptrdiff_t min_arity,
              ptrdiff_t max_arity, waitpid_module_function function,
              const char *doc)
{
  emacs_value symbol = env->intern(env, name);
  emacs_value func = env->make_function(env, min_arity, max_arity, function, doc,
                                        NULL);
  emacs_value args[] = { symbol, func };

  env->funcall(env, env->intern(env, "fset"), 2, args);
}

int
emacs_module_init(struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment(runtime);

  waitpid_defun(env, "emacs-waitpid", 2, 2, Fwaitpid,
                "Call waitpid(PID, &status, OPTIONS).\n\
Return a plist with :pid, :status, :errno, and :error.");
  waitpid_defun(env, "emacs-waitpid-reap-loop", 0, 1, Fwaitpid_reap_loop,
                "Repeatedly call waitpid(-1, &status, WNOHANG).\n\
Optional LIMIT bounds the number of children reaped.  Return the reap count.");

  emacs_value provide_args[] = { env->intern(env, "emacs-waitpid-module") };
  env->funcall(env, env->intern(env, "provide"), 1, provide_args);

  return 0;
}
