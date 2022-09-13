## Linking variables with different sizes

- Observation: `a.c` has `int x`, and `b.c` has `double x`. They are successfully merged by `gcc`...
- Conclusion: linker doesn't fail even if we merge two varible definitions with different sizes.
