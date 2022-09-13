pub type Matrix = Vec<Vec<f32>>;

/// Computes the product of the inputs `mat1` and `mat2`.
pub fn mat_mult(mat1: &Matrix, mat2: &Matrix) -> Matrix {
    let r = mat1.len();
    let c = mat2[0].len();
    let shared = mat2.len();
    let mut result = vec![vec![0f32; c]; r];
    for i in 0..r {
        for j in 0..c {
            let mut temp = 0f32;
            for k in 0..shared {
                temp += mat1[i][k] * mat2[k][j];
            }
            result[i][j] = temp;
        }
    }
    result
}
