rm -rf ./build
mkdir build
cd build
cmake ..

make starter_test
make lru_replacer_test
make b_plus_tree_insert_test -j
make b_plus_tree_delete_test -j
make b_plus_tree_concurrent_test -j
make executor_test
make lock_manager_test
make transaction_test

./test/starter_test
./test/lru_replacer_test
./test/b_plus_tree_insert_test
./test/b_plus_tree_delete_test
./test/b_plus_tree_concurrent_test
./test/executor_test
./test/lock_manager_test
./test/transaction_test