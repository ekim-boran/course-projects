//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// p0_starter.h
//
// Identification: src/include/primer/p0_starter.h
//
// Copyright (c) 2015-2020, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#pragma once

#include <memory>
#include <utility>
namespace bustub {

/*
 * The base class defining a Matrix
 */
template <typename T>
class Matrix {
 protected:
  // TODO(P0): Add implementation
  Matrix(int r, int c) {
    rows = r;
    cols = c;
    linear = new T[r * c];
  }

  // # of rows in the matrix
  int rows;
  // # of Columns in the matrix
  int cols;
  // Flattened array containing the elements of the matrix
  // TODO(P0) : Allocate the array in the constructor. Don't forget to free up
  // the array in the destructor.
  T *linear;

 public:
  // Return the # of rows in the matrix
  virtual int GetRows() = 0;

  // Return the # of columns in the matrix
  virtual int GetColumns() = 0;

  // Return the (i,j)th  matrix element
  virtual T GetElem(int i, int j) = 0;

  // Sets the (i,j)th  matrix element to val
  virtual void SetElem(int i, int j, T val) = 0;

  // Sets the matrix elements based on the array arr
  virtual void MatImport(T *arr) = 0;

  // TODO(P0): Add implementation
  virtual ~Matrix() = default;
};

template <typename T>
class RowMatrix : public Matrix<T> {
 public:
  // TODO(P0): Add implementation
  RowMatrix(int r, int c) : Matrix<T>(r, c) {
    data_ = new T *[r];
    for (int i = 0; i < this->rows; i++) {
      data_[i] = this->linear + (i * c);
    }
  }

  // TODO(P0): Add implementation

  int GetRows() override { return this->rows; }

  // TODO(P0): Add implementation
  int GetColumns() override { return this->cols; }

  // TODO(P0): Add implementation
  T GetElem(int i, int j) override { return data_[i][j]; }

  // TODO(P0): Add implementation
  void SetElem(int i, int j, T val) override { data_[i][j] = val; }

  // TODO(P0): Add implementation
  void MatImport(T *arr) override { memcpy(this->linear, arr, sizeof(T) * this->rows * this->cols); }

  // TODO(P0): Add implementation
  ~RowMatrix() override {
    delete[] this->linear;
    delete[] data_;
  }

 private:
  // 2D array containing the elements of the matrix in row-major format
  // TODO(P0): Allocate the array of row pointers in the constructor. Use these pointers
  // to point to corresponding elements of the 'linear' array.
  // Don't forget to free up the array in the destructor.
  T **data_;
};

template <typename T>
class RowMatrixOperations {
 public:
  // Compute (mat1 + mat2) and return the result.
  // Return nullptr if dimensions mismatch for input matrices.
  static std::unique_ptr<RowMatrix<T>> AddMatrices(std::unique_ptr<RowMatrix<T>> mat1,
                                                   std::unique_ptr<RowMatrix<T>> mat2) {
    int c = mat1->GetColumns();
    int r = mat1->GetRows();
    if (c != mat2->GetColumns() || r != mat2->GetRows()) {
      return std::unique_ptr<RowMatrix<T>>(nullptr);
    }
    std::unique_ptr<RowMatrix<int>> mat_result{new RowMatrix<int>(r, c)};

    for (int i = 0; i < r; i++) {
      for (int j = 0; j < c; j++) {
        mat_result->SetElem(i, j, mat1->GetElem(i, j) + mat2->GetElem(i, j));
      }
    }

    return mat_result;
  }

  // Compute matrix multiplication (mat1 * mat2) and return the result.
  // Return nullptr if dimensions mismatch for input matrices.
  static std::unique_ptr<RowMatrix<T>> MultiplyMatrices(std::unique_ptr<RowMatrix<T>> mat1,
                                                        std::unique_ptr<RowMatrix<T>> mat2) {
    int c1 = mat1->GetColumns();
    int r1 = mat1->GetRows();
    int c2 = mat2->GetColumns();
    int r2 = mat2->GetRows();
    if (c1 != r2) {
      return std::unique_ptr<RowMatrix<T>>(nullptr);
    }
    std::unique_ptr<RowMatrix<int>> mat_result{new RowMatrix<int>(r1, c2)};

    for (int i = 0; i < r1; i++) {
      for (int j = 0; j < c2; j++) {
        mat_result->SetElem(i, j, 0);
        for (int k = 0; k < c1; k++) {
          int old = mat_result->GetElem(i, j);
          mat_result->SetElem(i, j, old + (mat1->GetElem(i, k) * mat2->GetElem(k, j)));
        }
      }
    }

    return mat_result;
  }

  // Simplified GEMM (general matrix multiply) operation
  // Compute (matA * matB + matC). Return nullptr if dimensions mismatch for input matrices
  static std::unique_ptr<RowMatrix<T>> GemmMatrices(std::unique_ptr<RowMatrix<T>> matA,
                                                    std::unique_ptr<RowMatrix<T>> matB,
                                                    std::unique_ptr<RowMatrix<T>> matC) {
    // TODO(P0): Add code

    auto m1 = MultiplyMatrices(std::move(matA), std::move(matB));
    if (m1 == nullptr) {
      return m1;
    }
    return AddMatrices(std::move(m1), std::move(matC));
  }
};
}  // namespace bustub
