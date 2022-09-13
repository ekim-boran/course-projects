pub fn sieve(n: u32) -> Vec<u32> {
    let mut r = vec![true; n as usize];
    let mut result = vec![];
    for i in 2..n {
        if r[i as usize] {
            result.push(i);
            for k in 2.. {
                if i * k >= n {
                    break;
                }
                r[(i * k) as usize] = false;
            }
        }
    }
    result
}
