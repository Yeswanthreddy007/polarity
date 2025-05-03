#![allow(non_snake_case, non_camel_case_types, dead_code)]

/*
    Fill in the polarity function below. Use as many helpers as you want.
    Test your code by running 'cargo test' from the tester_rs_simple directory.
    
*/
fn polarity(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> Vec<String> {

    let total_rows = board.len();
    if total_rows == 0 {
        return vec![];
    }
    let total_cols = board[0].len();
    
    let orientation_grid: Vec<Vec<char>> = board.iter().map(|line| line.chars().collect()).collect();
    
    let initial_grid: Vec<Vec<Option<char>>> = vec![vec![None; total_cols]; total_rows];
    let initial_state = (
        initial_grid,
        vec![0; total_rows],
        vec![0; total_rows],
        vec![0; total_cols],
        vec![0; total_cols]
    );
    let constraints_data = specs;
    
    let mut domino_list: Vec<(usize, usize, usize, usize)> = Vec::new();
    for row_idx in 0..total_rows {
        for col_idx in 0..total_cols {
            let current_char = orientation_grid[row_idx][col_idx];
            if current_char == 'T' {
                if row_idx + 1 < total_rows {
                    domino_list.push((row_idx, col_idx, row_idx + 1, col_idx));
                }
            } else if current_char == 'L' {
                if col_idx + 1 < total_cols {
                    domino_list.push((row_idx, col_idx, row_idx, col_idx + 1));
                }
            }
        }
    }
    
    domino_list.sort_by(|&domino1, &domino2| weight(domino2, constraints_data).cmp(&weight(domino1, constraints_data)));
    
    if let Some(solution_state) = solve_dominoes(&initial_state, &domino_list, 0, total_rows, total_cols, constraints_data, &orientation_grid) {
        solution_state.0.into_iter().map(|row| {
            row.into_iter().map(|cell| cell.unwrap_or('X')).collect()
        }).collect()
    } else {
        board.iter().map(|line| "X".repeat(line.len())).collect()
    }
}

fn weight(domino: (usize, usize, usize, usize), constraints: &(Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> i32 {
    let (left_constraints, right_constraints, top_constraints, bottom_constraints) =
        (&constraints.0, &constraints.1, &constraints.2, &constraints.3);
    let (row1, col1, row2, col2) = domino;
    let mut domino_weight = 0;
    if left_constraints[row1] != -1 { domino_weight += 1; }
    if right_constraints[row1] != -1 { domino_weight += 1; }
    if left_constraints[row2] != -1 { domino_weight += 1; }
    if right_constraints[row2] != -1 { domino_weight += 1; }
    if top_constraints[col1] != -1 { domino_weight += 1; }
    if bottom_constraints[col1] != -1 { domino_weight += 1; }
    if top_constraints[col2] != -1 { domino_weight += 1; }
    if bottom_constraints[col2] != -1 { domino_weight += 1; }
    domino_weight
} 

fn solve_dominoes(
    current_state: & (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    domino_list: &Vec<(usize, usize, usize, usize)>,
    current_index: usize,
    total_rows: usize,
    total_cols: usize,
    constraints_data: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    orientation_grid: &Vec<Vec<char>>
) -> Option<(Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)> {
    if current_index == domino_list.len() {
        if final_constraints_ok(current_state, total_rows, total_cols, constraints_data) {
            return Some(current_state.clone());
        } else {
            return None;
        }
    }
    let (row1, col1, row2, col2) = domino_list[current_index];
    if current_state.0[row1][col1].is_some() || current_state.0[row2][col2].is_some() {
        return solve_dominoes(current_state, domino_list, current_index + 1, total_rows, total_cols, constraints_data, orientation_grid);
    }
    let assignment_options = vec![ ('+', '-'), ('-', '+'), ('X', 'X') ];
    for assignment in assignment_options {
        if let Some(new_state) = try_assignment(
            current_state,
            constraints_data,
            row1,
            col1,
            row2,
            col2,
            assignment,
            total_rows,
            total_cols
        ) {
            if let Some(solution) = solve_dominoes(&new_state, domino_list, current_index + 1, total_rows, total_cols, constraints_data, orientation_grid) {
                return Some(solution);
            }
        }
    }
    None
}

fn try_assignment(
    current_state: & (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    constraints_data: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    row1: usize,
    col1: usize,
    row2: usize,
    col2: usize,
    assignment: (char, char),
    total_rows: usize,
    total_cols: usize
) -> Option<(Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)> {
    let state_after_first = update_state(current_state, row1, col1, assignment.0, total_cols);
    let new_state = update_state(&state_after_first, row2, col2, assignment.1, total_cols);
    if valid_neighbors(&new_state.0, row1, col1, assignment.0, (row2, col2), total_rows, total_cols) &&
       valid_neighbors(&new_state.0, row2, col2, assignment.1, (row1, col1), total_rows, total_cols) &&
       partial_constraints_ok(&new_state, total_rows, total_cols, constraints_data)
    {
        Some(new_state)
    } else {
        None
    }
}

fn update_state(
    current_state: & (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    row: usize,
    col: usize,
    value: char,
    _total_cols: usize
) -> (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>) {
    let mut grid = current_state.0.clone();
    grid[row][col] = Some(value);
    
    let mut row_positive = current_state.1.clone();
    let mut row_negative = current_state.2.clone();
    let mut col_positive = current_state.3.clone();
    let mut col_negative = current_state.4.clone();
    
    let (delta_positive, delta_negative) = match value {
         '+' => (1, 0),
         '-' => (0, 1),
         'X' => (0, 0),
         _   => (0, 0)
    };
    row_positive[row] += delta_positive;
    row_negative[row] += delta_negative;
    col_positive[col] += delta_positive;
    col_negative[col] += delta_negative;
    
    (grid, row_positive, row_negative, col_positive, col_negative)
}

fn valid_neighbors(
    grid: &Vec<Vec<Option<char>>>,
    row: usize,
    col: usize,
    value: char,
    domino_partner: (usize, usize),
    total_rows: usize,
    total_cols: usize
) -> bool {
    if value == 'X' {
        return true;
    }
    let directions = vec![(-1isize, 0isize), (1, 0), (0, -1), (0, 1)];
    for (d_row, d_col) in directions {
        let neighbor_row = row as isize + d_row;
        let neighbor_col = col as isize + d_col;
        if neighbor_row < 0 || neighbor_row >= total_rows as isize || neighbor_col < 0 || neighbor_col >= total_cols as isize {
            continue;
        }
        let neighbor_row = neighbor_row as usize;
        let neighbor_col = neighbor_col as usize;
        if (neighbor_row, neighbor_col) == domino_partner {
            continue;
        }
        if let Some(adjacent_value) = grid[neighbor_row][neighbor_col] {
            if adjacent_value != 'X' && adjacent_value == value {
                return false;
            }
        }
    }
    true
}

fn partial_constraints_ok(
    current_state: & (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    total_rows: usize,
    total_cols: usize,
    constraints_data: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)
) -> bool {
    let (grid, row_positive, row_negative, col_positive, col_negative) = current_state;
    let (left_constraints, right_constraints, top_constraints, bottom_constraints) =
        (&constraints_data.0, &constraints_data.1, &constraints_data.2, &constraints_data.3);
    
    for row in 0..total_rows {
        let full_count = total_cols as i32;
        let assigned_count = grid[row].iter().filter(|cell| cell.is_some()).count() as i32;
        let remaining_count = full_count - assigned_count;
        let left_requirement = left_constraints[row];
        let right_requirement = right_constraints[row];
        if left_requirement != -1 {
            if row_positive[row] > left_requirement || row_positive[row] + remaining_count < left_requirement {
                return false;
            }
        }
        if right_requirement != -1 {
            if row_negative[row] > right_requirement || row_negative[row] + remaining_count < right_requirement {
                return false;
            }
        }
    }
    for col in 0..total_cols {
        let full_count = total_rows as i32;
        let assigned_count = grid.iter().filter(|row| row[col].is_some()).count() as i32;
        let remaining_count = full_count - assigned_count;
        let top_requirement = top_constraints[col];
        let bottom_requirement = bottom_constraints[col];
        if top_requirement != -1 {
            if col_positive[col] > top_requirement || col_positive[col] + remaining_count < top_requirement {
                return false;
            }
        }
        if bottom_requirement != -1 {
            if col_negative[col] > bottom_requirement || col_negative[col] + remaining_count < bottom_requirement {
                return false;
            }
        }
    }
    true
}

fn final_constraints_ok(
    current_state: & (Vec<Vec<Option<char>>>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>),
    total_rows: usize,
    total_cols: usize,
    constraints_data: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)
) -> bool {
    let (grid, row_positive, row_negative, col_positive, col_negative) = current_state;
    let (left_constraints, right_constraints, top_constraints, bottom_constraints) =
        (&constraints_data.0, &constraints_data.1, &constraints_data.2, &constraints_data.3);
    
    for row in 0..total_rows {
        let assigned_cells = grid[row].iter().filter(|cell| cell.is_some()).count();
        if assigned_cells != total_cols {
            return false;
        }
        if left_constraints[row] != -1 && row_positive[row] != left_constraints[row] {
            return false;
        }
        if right_constraints[row] != -1 && row_negative[row] != right_constraints[row] {
            return false;
        }
    }
    for col in 0..total_cols {
        let assigned_cells = grid.iter().filter(|row| row[col].is_some()).count();
        if assigned_cells != total_rows {
            return false;
        }
        if top_constraints[col] != -1 && col_positive[col] != top_constraints[col] {
            return false;
        }
        if bottom_constraints[col] != -1 && col_negative[col] != bottom_constraints[col] {
            return false;
        }
    }
    true
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;
