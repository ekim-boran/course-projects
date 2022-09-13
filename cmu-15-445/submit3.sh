#cd build
#make -j
#make format -j
#make check-lint -j
#make check-clang-tidy -j
#cd ..

zip p3.zip \
    ./src/include/buffer/lru_replacer.h \
    ./src/buffer/lru_replacer.cpp \
    ./src/include/buffer/buffer_pool_manager.h \
    ./src/buffer/buffer_pool_manager.cpp \
    ./src/include/storage/page/b_plus_tree_page.h     


zip p3.zip \
    ./src/include/storage/page/b_plus_tree_leaf_page.h   \
      ./src/include/storage/page/b_plus_tree_internal_page.h \
      ./src/storage/page/b_plus_tree_leaf_page.cpp       \
     ./src/include/storage/index/index_iterator.h         \
     ./src/storage/index/b_plus_tree.cpp         \
     ./src/storage/index/index_iterator.cpp       \
     ./src/storage/page/b_plus_tree_page.cpp           \
    ./src/storage/page/b_plus_tree_internal_page.cpp   \
    ./src/include/storage/index/b_plus_tree.h         \

zip p3.zip \
    src/include/catalog/catalog.h \
    src/include/execution/execution_engine.h \
    src/include/execution/executor_factory.h \
    src/include/execution/executors/seq_scan_executor.h \
    src/include/execution/executors/index_scan_executor.h \
    src/include/execution/executors/insert_executor.h \
    src/include/execution/executors/update_executor.h \
    src/include/execution/executors/delete_executor.h \
    src/include/execution/executors/nested_loop_join_executor.h \
    src/include/execution/executors/nested_index_join_executor.h \
    src/include/execution/executors/limit_executor.h \
    src/include/execution/executors/aggregation_executor.h \
    src/include/storage/index/b_plus_tree_index.h \
    src/include/storage/index/index.h \
    src/execution/executor_factory.cpp \
    src/execution/seq_scan_executor.cpp \
    src/execution/index_scan_executor.cpp \
    src/execution/insert_executor.cpp \
    src/execution/update_executor.cpp \
    src/execution/delete_executor.cpp \
    src/execution/nested_loop_join_executor.cpp \
    src/execution/nested_index_join_executor.cpp \
    src/execution/limit_executor.cpp \
    src/execution/aggregation_executor.cpp \
    src/storage/index/b_plus_tree_index.cpp  \