#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

#define STUDENT "251ADB063"
#define SUFFIX "_ali_sattarov_" STUDENT ".txt"
//Ali Sattarov - Ελληνική
//Έχει κατανοήσει βασικά και κρίσιμα
typedef struct { long long i; double d; int real; size_t start; } Value;
typedef struct { int kind; size_t pos; Value v; } Token;
enum { END = 256, NUMBER, POWER, BAD };
typedef struct {
    const char *text;
    size_t length, cursor, base, error;
    Token token;
} Parser;
//Ελληνική
// Allocateee memory and stop if it fail
static void *allocate(void *old, size_t count, size_t size)
{
    if (count > (size_t)-1 / size) {
        fputs("Allocation size overflow\n", stderr); exit(EXIT_FAILURE);
    }
    void *p = realloc(old, count * size);
    if (!p) { fputs("Out of memory\n", stderr); exit(EXIT_FAILURE); }
    return p;
}

static void fail(Parser *p, size_t pos)
{
    if (!p->error) p->error = pos;
}

static void next(Parser *p)
{
    while (p->cursor < p->length &&
           (p->text[p->cursor] == ' ' || p->text[p->cursor] == '\t'))
        ++p->cursor;
    size_t begin = p->cursor;
    p->token.pos = p->base + begin + 1;
    p->token.kind = END;
    if (begin == p->length) return;
    unsigned char c = (unsigned char)p->text[p->cursor++];
    if (isdigit(c)) {
        while (p->cursor < p->length &&
               isdigit((unsigned char)p->text[p->cursor])) ++p->cursor;
        int real = 0;
        if (p->cursor < p->length && p->text[p->cursor] == '.') {
            size_t dot = p->cursor++;
            if (p->cursor == p->length ||
                !isdigit((unsigned char)p->text[p->cursor])) {
                p->token.kind = BAD;
                p->token.pos = p->base + dot + 1;
                return;
            }
            real = 1;
            while (p->cursor < p->length &&
                   isdigit((unsigned char)p->text[p->cursor])) ++p->cursor;
        }
        Value v = {0, 0.0, real, p->base + begin + 1};
        if (real) {
            size_t n = p->cursor - begin;
            char *number = allocate(NULL, n + 1, 1);
            memcpy(number, p->text + begin, n); number[n] = '\0';
            v.d = strtod(number, NULL); free(number);
            if (!isfinite(v.d)) { p->token.kind = BAD; return; }
        } else {
            v.i = strtoll(p->text + begin, NULL, 10);
        }
        // STORES the number in the current token.
        //Ελληνική
        p->token.kind = NUMBER; p->token.v = v;
    } else if (c == '*' && p->cursor < p->length &&
               p->text[p->cursor] == '*') {
        ++p->cursor; p->token.kind = POWER;
    } else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')') {
        p->token.kind = c;
    } else p->token.kind = BAD;
}

static Value apply(Parser *p, Value a, Value b, int op, size_t pos)
{
    Value r = {0, 0.0, a.real || b.real, a.start};
    if (p->error) return r;
    if (op == '/' && (b.real ? b.d == 0.0 : b.i == 0)) {
        fail(p, b.start); return r;
    }
    if (r.real) {
        double x = a.real ? a.d : (double)a.i;
        double y = b.real ? b.d : (double)b.i;
        switch (op) {
        case '+': r.d = x + y; break;
        case '-': r.d = x - y; break;
        case '*': r.d = x * y; break;
        case '/': r.d = x / y; break;
        default: r.d = pow(x, y); break;
        }
        if (!isfinite(r.d)) fail(p, pos);
    } else {
        switch (op) {
        case '+': r.i = a.i + b.i; break;
        case '-': r.i = a.i - b.i; break;
        case '*': r.i = a.i * b.i; break;
        case '/': r.i = a.i / b.i; break;
        default:
            if (b.i < 0) { fail(p, b.start); break; }
            r.i = 1;
            for (long long x = a.i, n = b.i; n; ) {
                if (n & 1) r.i *= x;
                n /= 2;
                if (n) x *= x;
            }
        }
    }
    return r;
}
// Reads numbers or expression in parentheses.
static Value expression(Parser *p);
//ΝΑΙ (στην Αγγλική)
static Value primary(Parser *p)
{
    Value v = {0, 0.0, 0, p->token.pos};
    if (p->error) return v;
    if (p->token.kind == NUMBER) {
        v = p->token.v; next(p); return v;
    }
    if (p->token.kind == '(') {
        size_t start = p->token.pos;
        next(p); v = expression(p);
        if (!p->error) {
            if (p->token.kind != ')') fail(p, p->token.pos);
            else next(p);
        }
        v.start = start;
    } else fail(p, p->token.pos);
    return v;
}

static Value power(Parser *p)
{
    typedef struct { Value v; size_t op; } Part;
    Value first = primary(p);
    if (p->error || p->token.kind != POWER) return first;
    size_t n = 1, cap = 8;
    Part *parts = allocate(NULL, cap, sizeof *parts);
    parts[0].v = first;
    while (!p->error && p->token.kind == POWER) {
        parts[n - 1].op = p->token.pos;
        next(p);
        if (n == cap) { cap *= 2; parts = allocate(parts, cap, sizeof *parts); }
        parts[n++].v = primary(p);
    }
    Value v = parts[n - 1].v;
    while (!p->error && n > 1) {
        --n; v = apply(p, parts[n - 1].v, v, POWER, parts[n - 1].op);
    }
    free(parts); return v;
}
//ΝΑΙ (στην Αγγλική)

static Value term(Parser *p)
{
    Value v = power(p);
    while (!p->error && (p->token.kind == '*' || p->token.kind == '/')) {
        int op = p->token.kind; size_t pos = p->token.pos;
        next(p); Value rhs = power(p); v = apply(p, v, rhs, op, pos);
    }
    return v;
}

static Value expression(Parser *p)
{
    Value v = term(p);
    while (!p->error && (p->token.kind == '+' || p->token.kind == '-')) {
        int op = p->token.kind; size_t pos = p->token.pos;
        next(p); Value rhs = term(p); v = apply(p, v, rhs, op, pos);
    }
    return v;
    // Join folder path and file name.
}
//ΝΑΙ (στην Αγγλική)
static char *join(const char *dir, const char *name)
{
    size_t a = strlen(dir), b = strlen(name);
    char *s = allocate(NULL, a + b + 2, 1);
    memcpy(s, dir, a); s[a] = '/'; memcpy(s + a + 1, name, b + 1); return s;
}

static char *basename_copy(const char *path, int strip_txt)
{
    size_t end = strlen(path);
    while (end > 1 && path[end - 1] == '/') --end;
    size_t begin = end;
    while (begin && path[begin - 1] != '/') --begin;
    size_t n = end - begin;
    if (strip_txt && n >= 4 && !memcmp(path + end - 4, ".txt", 4)) n -= 4;
    if (!n) return memcpy(allocate(NULL, 6, 1), "input", 6);
    char *s = allocate(NULL, n + 1, 1);
    memcpy(s, path + begin, n); s[n] = '\0'; return s;
}

static int process(const char *input, const char *outdir)
{
    FILE *in = fopen(input, "rb");
    if (!in) { perror(input); return 1; }
    FILE *results = tmpfile();
    if (!results) { perror("temporary results"); fclose(in); return 1; }
    char *line = NULL; size_t cap = 0, base = 0, error = 0;
    int status = 0, eof = 0;
    while (!eof && !error && !status) {
        size_t n = 0; int c;
        while ((c = fgetc(in)) != EOF && c != '\n') {
            if (n + 1 >= cap) { cap = cap ? cap * 2 : 256; line = allocate(line, cap, 1); }
            line[n++] = (char)c;
        }
        eof = c == EOF;
        if (ferror(in)) { perror(input); status = 1; break; }
        if (!line) { cap = 256; line = allocate(NULL, cap, 1); }
        line[n] = '\0';
        Parser p = {line, n, 0, base, 0, {0, 0, {0, 0.0, 0, 0}}};
        size_t first = 0;
        while (first < n && (line[first] == ' ' || line[first] == '\t')) ++first;
        if (first < n && line[first] != '#') {
            next(&p); Value v = expression(&p);
            if (!p.error && p.token.kind != END) fail(&p, p.token.pos);
            error = p.error;
            if (!error) {
                int written = v.real ? fprintf(results, "%.15g\n", v.d) : fprintf(results, "%lld\n", v.i);
                if (written < 0) { perror("temporary results"); status = 1; }
            }
            // The next line start at updated byte position.
        }
        base += n + (c == '\n');
        //στην Αγγλική
    }
    free(line);
    if (fclose(in)) { perror(input); status = 1; }
    char *stem = basename_copy(input, 1);
    char *name = allocate(NULL, strlen(stem) + sizeof SUFFIX, 1);
    strcpy(name, stem); strcat(name, SUFFIX); free(stem);
    char *output = join(outdir, name); free(name);
    FILE *out = status ? NULL : fopen(output, "wb");
    if (!status && !out) { perror(output); status = 1; }
    if (out) {
        if (error) {
            if (fprintf(out, "ERROR:%zu\n", error) < 0) status = 1;
        } else {
            if (fflush(results) || fseek(results, 0, SEEK_SET)) status = 1;
            char buffer[8192]; size_t n;
            while (!status && (n = fread(buffer, 1, sizeof buffer, results)) != 0)
                if (fwrite(buffer, 1, n, out) != n) status = 1;
            if (ferror(results)) status = 1;
        }
        if (fclose(out)) status = 1;
        if (status) fprintf(stderr, "Cannot write output: %s\n", output);
    }
    fclose(results); free(output); return status;
}

static int usage(void)
{
    // Read program option and process  input file.
    //στην Αγγλική

    fputs("Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n", stderr);
    return 1;
}
//στην Αγγλική
int main(int argc, char **argv)
{
    const char *dir = NULL, *out = NULL, *input = NULL;
    for (int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "-d") || !strcmp(argv[i], "--dir")) {
            if (dir || i + 1 == argc) return usage();
            dir = argv[++i];
        } else if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output-dir")) {
            if (out || i + 1 == argc) return usage();
            out = argv[++i];
        } else if (argv[i][0] == '-' || input) return usage();
        else input = argv[i];
    }
    if ((!input && !dir) || (input && dir)) return usage();
    struct stat st;
    const char *source = dir ? dir : input;
    if (stat(source, &st)) { perror(source); return 1; }
    if ((dir && !S_ISDIR(st.st_mode)) || (input && !S_ISREG(st.st_mode))) {
        fprintf(stderr, "Invalid input type: %s\n", source); return 1;
    }
    char **files = NULL; size_t count = 0, capacity = 0;
    if (dir) {
        DIR *dp = opendir(dir);
        if (!dp) { perror(dir); return 1; }
        struct dirent *entry; int scan_error = 0;
        for (;;) {
            errno = 0; entry = readdir(dp);
            if (!entry) { if (errno) { perror(dir); scan_error = 1; } break; }
            size_t n = strlen(entry->d_name);
            if (n < 4 || strcmp(entry->d_name + n - 4, ".txt")) continue;
            char *path = join(dir, entry->d_name);
            if (stat(path, &st)) { perror(path); free(path); scan_error = 1; break; }
            if (!S_ISREG(st.st_mode)) { free(path); continue; }
            if (count == capacity) { capacity = capacity ? capacity * 2 : 16; files = allocate(files, capacity, sizeof *files); }
            files[count++] = path;
    //στην Αγγλική

        }
        if (closedir(dp)) { perror(dir); scan_error = 1; }
        if (scan_error) { for (size_t i = 0; i < count; ++i) free(files[i]); free(files); return 1; }
    }
    char *default_out = NULL;
    if (!out) {
        char *stem = basename_copy(source, !dir);
        default_out = allocate(NULL, strlen(stem) + sizeof "_ali_" STUDENT, 1);
        strcpy(default_out, stem); strcat(default_out, "_ali_" STUDENT);
        free(stem); out = default_out;
        //στην Αγγλική

    }
    int status = 0;
    if (mkdir(out, 0777) && errno != EEXIST) { perror(out); status = 1; }
    if (!status && (stat(out, &st) || !S_ISDIR(st.st_mode))) {
        fprintf(stderr, "Output is not a directory: %s\n", out); status = 1;
    }
    if (!status) {
        if (!dir) status = process(input, out);
        else for (size_t i = 0; i < count; ++i) if (process(files[i], out)) status = 1;
    }
    for (size_t i = 0; i < count; ++i) free(files[i]);
    free(files); free(default_out); return status;
}