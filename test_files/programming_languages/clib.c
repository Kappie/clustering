
//
// clib-install.c
//
// Copyright (c) 2012-2014 clib authors
// MIT licensed
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "asprintf/asprintf.h"
#include "fs/fs.h"
#include "tempdir/tempdir.h"
#include "commander/commander.h"
#include "clib-package/clib-package.h"
#include "http-get/http-get.h"
#include "logger/logger.h"
#include "debug/debug.h"
#include "version.h"

debug_t debugger;

struct options {
  const char *dir;
  int verbose;
  int dev;
};

static struct options opts;

/**
 * Option setters.
 */

static void
setopt_dir(command_t *self) {
  opts.dir = (char *) self->arg;
  debug(&debugger, "set dir: %s", opts.dir);
}

static void
setopt_quiet(command_t *self) {
  opts.verbose = 0;
  debug(&debugger, "set quiet flag");
}

static void
setopt_dev(command_t *self) {
  opts.dev = 1;
  debug(&debugger, "set development flag");
}

/**
 * Install dependency packages at `pwd`.
 */

static int
install_local_packages() {
  if (-1 == fs_exists("./package.json")) {
    logger_error("error", "Missing package.json");
    return 1;
  }

  debug(&debugger, "reading local package.json");
  char *json = fs_read("./package.json");
  if (NULL == json) return 1;

  clib_package_t *pkg = clib_package_new(json, opts.verbose);
  if (NULL == pkg) goto e1;

  int rc = clib_package_install_dependencies(pkg, opts.dir, opts.verbose);
  if (-1 == rc) goto e2;

  if (opts.dev) {
    rc = clib_package_install_development(pkg, opts.dir, opts.verbose);
    if (-1 == rc) goto e2;
  }

  free(json);
  clib_package_free(pkg);
  return 0;

e2:
  clib_package_free(pkg);
e1:
  free(json);
  return 1;
}

#define E_FORMAT(...) ({      \
  rc = asprintf(__VA_ARGS__); \
  if (-1 == rc) goto cleanup; \
});

static int
executable(clib_package_t *pkg) {
  int rc;
  char *url = NULL;
  char *file = NULL;
  char *tarball = NULL;
  char *command = NULL;
  char *dir = NULL;
  char *deps = NULL;
  char *tmp = NULL;
  char *reponame = NULL;

  debug(&debugger, "install executable %s", pkg->repo);

  tmp = gettempdir();
  if (NULL == tmp) {
    logger_error("error", "gettempdir() out of memory");
    return -1;
  }

  if (!pkg->repo) {
    logger_error("error", "repo field required to install executable");
    return -1;
  }

  reponame = strrchr(pkg->repo, '/');
  if (reponame && *reponame != '\0') reponame++;
  else {
    logger_error("error", "malformed repo field, must be in the form user/pkg");
    return -1;
  }

  E_FORMAT(&url
    , "https://github.com/%s/archive/%s.tar.gz"
    , pkg->repo
    , pkg->version);
  E_FORMAT(&file, "%s-%s.tar.gz", reponame, pkg->version);
  E_FORMAT(&tarball, "%s/%s", tmp, file);
  rc = http_get_file(url, tarball);
  E_FORMAT(&command, "cd %s && gzip -dc %s | tar x", tmp, file);

  debug(&debugger, "download url: %s", url);
  debug(&debugger, "file: %s", file);
  debug(&debugger, "tarball: %s", tarball);
  debug(&debugger, "command: %s", command);

  // cheap untar
  rc = system(command);
  if (0 != rc) goto cleanup;

  E_FORMAT(&dir, "%s/%s-%s", tmp, reponame, pkg->version);
  debug(&debugger, "dir: %s", dir);

  if (pkg->dependencies) {
    E_FORMAT(&deps, "%s/deps", dir);
    debug(&debugger, "deps: %s", deps);
    rc = clib_package_install_dependencies(pkg, deps, opts.verbose);
    if (-1 == rc) goto cleanup;
  }

  free(command);
  command = NULL;

  E_FORMAT(&command, "cd %s && %s", dir, pkg->install);
  debug(&debugger, "command: %s", command);
  rc = system(command);

cleanup:
  free(tmp);
  free(dir);
  free(command);
  free(tarball);
  free(file);
  free(url);
  return rc;
}

#undef E_FORMAT

/**
 * Create and install a package from `slug`.
 */

static int
install_package(const char *slug) {
  int rc;

  clib_package_t *pkg = clib_package_new_from_slug(slug, opts.verbose);
  if (NULL == pkg) return -1;

  if (pkg->install) {
    rc = executable(pkg);
    goto done;
  }

  rc = clib_package_install(pkg, opts.dir, opts.verbose);
  if (0 == rc && opts.dev) {
    rc = clib_package_install_development(pkg, opts.dir, opts.verbose);
  }

done:
  clib_package_free(pkg);
  return rc;
}

/**
 * Install the given `pkgs`.
 */

static int
install_packages(int n, char *pkgs[]) {
  for (int i = 0; i < n; i++) {
    debug(&debugger, "install %s (%d)", pkgs[i], i);
    if (-1 == install_package(pkgs[i])) return 1;
  }
  return 0;
}

/**
 * Entry point.
 */

int
main(int argc, char *argv[]) {
#ifdef _WIN32
  opts.dir = ".\\deps";
#else
  opts.dir = "./deps";
#endif
  opts.verbose = 1;
  opts.dev = 0;

  debug_init(&debugger, "clib-install");

  command_t program;

  command_init(&program
    , "clib-install"
    , CLIB_VERSION);

  program.usage = "[options] [name ...]";

  command_option(&program
    , "-o"
    , "--out <dir>"
    , "change the output directory [deps]"
    , setopt_dir);
  command_option(&program
    , "-q"
    , "--quiet"
    , "disable verbose output"
    , setopt_quiet);
  command_option(&program
    , "-d"
    , "--dev"
    , "install development dependencies"
    , setopt_dev);
  command_parse(&program, argc, argv);

  debug(&debugger, "%d arguments", program.argc);

  int code = 0 == program.argc
    ? install_local_packages()
    : install_packages(program.argc, program.argv);

  command_free(&program);
  return code;
}

//
// clib-search.c
//
// Copyright (c) 2012-2014 clib authors
// MIT licensed
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "case/case.h"
#include "commander/commander.h"
#include "tempdir/tempdir.h"
#include "fs/fs.h"
#include "http-get/http-get.h"
#include "asprintf/asprintf.h"
#include "wiki-registry/wiki-registry.h"
#include "clib-package/clib-package.h"
#include "console-colors/console-colors.h"
#include "strdup/strdup.h"
#include "logger/logger.h"
#include "debug/debug.h"
#include "version.h"

#define CLIB_WIKI_URL "https://github.com/clibs/clib/wiki/Packages"
#define CLIB_SEARCH_CACHE "clib-search.cache"
#define CLIB_SEARCH_CACHE_TIME 1000 * 60 * 60 * 5

debug_t debugger;

static int opt_color;

static void
setopt_nocolor(command_t *self) {
    opt_color = 0;
}

static int
matches(int count, char *args[], wiki_package_t *pkg) {
  // Display all packages if there's no query
  if (0 == count) return 1;

  char *name = NULL;
  char *description = NULL;

  name = clib_package_parse_name(pkg->repo);
  if (NULL == name) goto fail;
  case_lower(name);
  for (int i = 0; i < count; i++) {
    if (strstr(name, args[i])) {
      free(name);
      return 1;
    }
  }

  description = strdup(pkg->description);
  if (NULL == description) goto fail;
  case_lower(description);
  for (int i = 0; i < count; i++) {
    if (strstr(description, args[i])) {
      free(description);
      free(name);
      return 1;
    }
  }

fail:
  free(name);
  free(description);
  return 0;
}

static char *
clib_search_file(void) {
  char *file = NULL;
  char *temp = NULL;

  temp = gettempdir();
  if (NULL == temp) {
    logger_error("error", "gettempdir() out of memory");
    return NULL;
  }

  debug(&debugger, "tempdir: %s", temp);
  int rc = asprintf(&file, "%s/%s", temp, CLIB_SEARCH_CACHE);
  if (-1 == rc) {
    logger_error("error", "asprintf() out of memory");
    free(temp);
    return NULL;
  }

  free(temp);
  debug(&debugger, "search file: %s", file);
  return file;
}

static char *
wiki_html_cache() {
  char *cache_file = clib_search_file();
  if (NULL == cache_file) return NULL;

  fs_stats *stats = fs_stat(cache_file);
  if (NULL == stats) goto set_cache;

  long now = (long) time(NULL);
  long modified = stats->st_mtime;
  long delta = now - modified;

  debug(&debugger, "cache delta %d (%d - %d)", delta, now, modified);
  free(stats);

  if (delta < CLIB_SEARCH_CACHE_TIME) {
    char *data = fs_read(cache_file);
    free(cache_file);
    return data;
  }

set_cache:;
  debug(&debugger, "setting cache (%s) from %s", cache_file, CLIB_WIKI_URL);
  http_get_response_t *res = http_get(CLIB_WIKI_URL);
  if (!res->ok) return NULL;

  char *html = strdup(res->data);
  if (NULL == html) return NULL;
  http_get_free(res);

  if (NULL == html) return html;
  fs_write(cache_file, html);
  debug(&debugger, "wrote cache (%s)", cache_file);
  free(cache_file);
  return html;
}

int
main(int argc, char *argv[]) {
  opt_color = 1;

  debug_init(&debugger, "clib-search");

  command_t program;
  command_init(&program, "clib-search", CLIB_VERSION);
  program.usage = "[options] [query ...]";

  command_option(&program
    , "-n"
    , "--no-color"
    , "don't colorize output"
    , setopt_nocolor);

  command_parse(&program, argc, argv);

  for (int i = 0; i < program.argc; i++) case_lower(program.argv[i]);

  // set color theme
  cc_color_t fg_color_highlight = opt_color
    ? CC_FG_DARK_CYAN
    : CC_FG_NONE;
  cc_color_t fg_color_text = opt_color
    ? CC_FG_DARK_GRAY
    : CC_FG_NONE;

  char *html = wiki_html_cache();
  if (NULL == html) {
    command_free(&program);
    logger_error("error", "failed to fetch wiki HTML");
    return 1;
  }

  list_t *pkgs = wiki_registry_parse(html);
  free(html);

  debug(&debugger, "found %zu packages", pkgs->len);

  list_node_t *node;
  list_iterator_t *it = list_iterator_new(pkgs, LIST_HEAD);
  printf("\n");
  while ((node = list_iterator_next(it))) {
    wiki_package_t *pkg = (wiki_package_t *) node->val;
    if (matches(program.argc, program.argv, pkg)) {
      cc_fprintf(fg_color_highlight, stdout, "  %s\n", pkg->repo);
      printf("  url: ");
      cc_fprintf(fg_color_text, stdout, "%s\n", pkg->href);
      printf("  desc: ");
      cc_fprintf(fg_color_text, stdout, "%s\n", pkg->description);
      printf("\n");
    } else {
      debug(&debugger, "skipped package %s", pkg->repo);
    }
    wiki_package_free(pkg);
  }
  list_iterator_destroy(it);
  list_destroy(pkgs);
  command_free(&program);
  return 0;
}

//
// clib.c
//
// Copyright (c) 2012-2014 clib authors
// MIT licensed
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "trim/trim.h"
#include "asprintf/asprintf.h"
#include "which/which.h"
#include "str-flatten/str-flatten.h"
#include "strdup/strdup.h"
#include "debug/debug.h"
#include "version.h"

debug_t debugger;

static const char *usage =
  "\n"
  "  clib <command> [options]\n"
  "\n"
  "  Options:\n"
  "\n"
  "    -h, --help     Output this message\n"
  "    -v, --version  Output version information\n"
  "\n"
  "  Commands:\n"
  "\n"
  "    install [name...]  Install one or more packages\n"
  "    search [query]     Search for packages\n"
  "    help <cmd>         Display help for cmd\n"
  "";

#define format(...) ({                               \
  if (-1 == asprintf(__VA_ARGS__)) {                 \
    rc = 1;                                          \
    fprintf(stderr, "Memory allocation failure\n");  \
    goto cleanup;                                    \
  }                                                  \
})

int
main(int argc, const char **argv) {
  char *cmd = NULL;
  char *args = NULL;
  char *command = NULL;
  char *command_with_args = NULL;
  char *bin = NULL;
  int rc = 1;

  debug_init(&debugger, "clib");

  // usage
  if (NULL == argv[1]
   || 0 == strncmp(argv[1], "-h", 2)
   || 0 == strncmp(argv[1], "--help", 6)) {
    printf("%s\n", usage);
    return 0;
  }

  // version
  if (0 == strncmp(argv[1], "-v", 2)
   || 0 == strncmp(argv[1], "--version", 9)) {
    printf("%s\n", CLIB_VERSION);
    return 0;
  }

  // unknown
  if (0 == strncmp(argv[1], "--", 2)) {
    fprintf(stderr, "Unknown option: \"%s\"\n", argv[1]);
    return 1;
  }

  // sub-command
  cmd = strdup(argv[1]);
  if (NULL == cmd) {
    fprintf(stderr, "Memory allocation failure\n");
    return 1;
  }
  cmd = trim(cmd);

  if (0 == strcmp(cmd, "help")) {
    if (argc >= 3) {
      free(cmd);
      cmd = strdup(argv[2]);
      args = strdup("--help");
    } else {
      fprintf(stderr, "Help command required.\n");
      goto cleanup;
    }
  } else {
    if (argc >= 3) {
      args = str_flatten(argv, 2, argc);
      if (NULL == args) goto cleanup;
    }
  }
  debug(&debugger, "args: %s", args);

#ifdef _WIN32
  format(&command, "clib-%s.exe", cmd);
#else
  format(&command, "clib-%s", cmd);
#endif
  debug(&debugger, "command '%s'", cmd);

  bin = which(command);
  if (NULL == bin) {
    fprintf(stderr, "Unsupported command \"%s\"\n", cmd);
    goto cleanup;
  }

#ifdef _WIN32
  for (char *p = bin; *p; p++)
    if (*p == '/') *p = '\\';
#endif

  if (args) {
    format(&command_with_args, "%s %s", bin, args);
  } else {
    format(&command_with_args, "%s", bin);
  }

  debug(&debugger, "exec: %s", command_with_args);

  rc = system(command_with_args);
  debug(&debugger, "returned %d", rc);
  if (rc > 255) rc = 1;

cleanup:
  free(cmd);
  free(args);
  free(command);
  free(command_with_args);
  free(bin);
  return rc;
}
