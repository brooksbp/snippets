#ifndef ATOM_H
#define ATOM_H

// An Atom is a pointer to a unique, immutable sequence
// of 0+ bytes stored exactly once in an atom table.

extern int Atom_length(const char *str);

extern const char *Atom_new(const char *str, int len);

extern const char *Atom_string(const char *str);

extern const char *Atom_int(long n);

#endif ATOM_H
