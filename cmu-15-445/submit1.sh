cd build
make -j
make format -j
make check-lint -j
make check-clang-tidy -j
cd ..
zip project1-submission.zip \
    src/include/buffer/lru_replacer.h \
    src/buffer/lru_replacer.cpp \
    src/include/buffer/buffer_pool_manager.h \
    src/buffer/buffer_pool_manager.cpp \

#cd build
#
valgrind --trace-children=yes \
   --leak-check=full \
   --track-origins=yes \
   --soname-synonyms=somalloc=*jemalloc* \
   --error-exitcode=1 \
     ./test/buffer_pool_manager_test