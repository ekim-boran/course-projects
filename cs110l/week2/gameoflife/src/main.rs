use core::time::Duration;
use nannou::prelude::*;

fn main() {
    nannou::app(model)
        .update(update)
        .simple_window(view)
        .size(500, 500)
        .run();
}

struct Model {
    grid: Vec<Vec<bool>>,
    count: usize,
}

fn model(_app: &App) -> Model {
    // does not work
    _app.set_loop_mode(LoopMode::Rate {
        update_interval: Duration::from_secs(2),
    });
    let mut v = vec![vec![false; 20]; 20];
    v[2][0] = true;
    v[2][1] = true;
    v[2][2] = true;
    v[1][2] = true;
    v[0][1] = true;

    Model { grid: v, count: 0 }
}

fn update(_app: &App, _model: &mut Model, _update: Update) {
    _model.count += 1;

    if _model.count % 30 != 0 {
        return;
    }
    let mut alive_cells = vec![];
    for rowc in 0..20 {
        for colc in 0..20 {
            let count = neigbour_count(&_model.grid, rowc as isize, colc as isize);
            if _model.grid[rowc][colc] && (count == 2 || count == 3) {
                alive_cells.push((rowc, colc))
            } else if !(_model.grid[rowc][colc]) && (count == 3) {
                alive_cells.push((rowc, colc))
            } else {
            }
        }
    }
    for rowc in 0..20 {
        for colc in 0..20 {
            _model.grid[rowc][colc] = false;
        }
    }
    for (rowc, colc) in alive_cells {
        _model.grid[rowc][colc] = true;
    }
}

#[test]

fn test_nc() {
    let mut v = vec![vec![false; 20]; 20];
    v[2][0] = true;
    v[2][1] = true;
    v[2][2] = true;
    v[1][2] = true;
    v[0][1] = true;
    let c = neigbour_count(&v, 3, 1);
    println!("{}", c)
}

fn neigbour_count(grid: &Vec<Vec<bool>>, row: isize, col: isize) -> usize {
    [
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1),
    ]
    .iter()
    .map(|&(x, y)| grid[((x + 20) as usize) % 20][((y + 20) as usize) % 20])
    .filter(|x| *x)
    .count()
}

fn d(draw: &Draw, row: usize, col: usize) {
    let side = 25.0;
    let top_left = pt2(-250.0 + (col as f32) * side, 250.0 - ((row as f32) * side));
    let offset = vec2(side / 2.0, -side / 2.0);
    let xy = top_left + offset;
    draw.rect().xy(xy).w_h(side, side).color(PLUM);
}

fn view(_app: &App, _model: &Model, frame: Frame) {
    frame.clear(BLACK);
    let draw = _app.draw();
    for (rowc, row) in _model.grid.iter().enumerate() {
        for (colc, col) in row.iter().enumerate() {
            if *col {
                d(&draw, rowc, colc)
            }
        }
    }
    draw.to_frame(_app, &frame).unwrap();
}
