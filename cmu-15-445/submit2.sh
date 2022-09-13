cd build
#make -j
#make format -j
#make check-lint -j
#make check-clang-tidy -j
cd ..

sudo zip a.zip \
    ./src/include/buffer/lru_replacer.h \
    ./src/buffer/lru_replacer.cpp \
    ./src/include/buffer/buffer_pool_manager.h \
    ./src/buffer/buffer_pool_manager.cpp \
    ./src/include/storage/page/b_plus_tree_page.h     


sudo zip a.zip \
    ./src/include/storage/page/b_plus_tree_leaf_page.h   \
    ./src/include/storage/page/b_plus_tree_internal_page.h \
    ./src/storage/page/b_plus_tree_leaf_page.cpp       \
     ./src/include/storage/index/index_iterator.h         \
     ./src/storage/index/b_plus_tree.cpp         \
     ./src/storage/index/index_iterator.cpp       \
     ./src/storage/page/b_plus_tree_page.cpp           \
    ./src/storage/page/b_plus_tree_internal_page.cpp   \
    ./src/include/storage/index/b_plus_tree.h         \
