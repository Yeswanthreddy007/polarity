defmodule Polarity do
  @moduledoc """
  Solves the polarity puzzle using backtracking with incremental counters.

  """

  def polarity(input_board, input_specs) do
    # Convert input_board (tuple) to a 2D list of characters.
    my_orientation =
      input_board
      |> Tuple.to_list()
      |> Enum.map(&String.graphemes/1)

    num_rows = length(my_orientation)
    num_cols = if num_rows > 0, do: length(hd(my_orientation)), else: 0

    # Build an initial assignment board with nils.
    initial_assignment = for _ <- 1..num_rows, do: (for _ <- 1..num_cols, do: nil)

    # Initialize the state with the board and incremental counters.
    my_state = %{
      board: initial_assignment,
      row_plus: List.duplicate(0, num_rows),
      row_minus: List.duplicate(0, num_rows),
      col_plus: List.duplicate(0, num_cols),
      col_minus: List.duplicate(0, num_cols)
    }

    # Convert input_specs (tuples) to lists.
    my_constraints = %{
      left: Tuple.to_list(input_specs["left"]),
      right: Tuple.to_list(input_specs["right"]),
      top: Tuple.to_list(input_specs["top"]),
      bottom: Tuple.to_list(input_specs["bottom"])
    }

    # Precompute domino pairs from the orientation board.
    my_dominoes = domino_pairs(my_orientation)
    my_dominoes = sort_dominoes(my_dominoes, my_constraints)

    solved_state =
      solve_dominoes(my_state, my_dominoes, 0, num_rows, num_cols, my_constraints, my_orientation)

    if solved_state == :error do
      empty_solution(input_board)
    else
      board_to_tuple(solved_state.board)
    end
  end

  # Extract domino pairs from the orientation board.
  # For a cell "T", its partner is at {r+1, c} (should be "B").
  # For a cell "L", its partner is at {r, c+1} (should be "R").
  defp domino_pairs(my_orientation) do
    num_rows = length(my_orientation)
    num_cols = if num_rows > 0, do: length(hd(my_orientation)), else: 0

    my_dominoes =
      for r <- 0..(num_rows - 1), c <- 0..(num_cols - 1) do
        case Enum.at(Enum.at(my_orientation, r), c) do
          "T" -> [{r, c, r + 1, c}]
          "L" -> [{r, c, r, c + 1}]
          _   -> []
        end
      end

    List.flatten(my_dominoes)
  end

  # Sort dominoes using a simple weight based on how many constraints affect the domino’s rows/columns.
  defp sort_dominoes(my_dominoes, my_constraints) do
    Enum.sort(my_dominoes, fn d1, d2 ->
      domino_weight(d1, my_constraints) >= domino_weight(d2, my_constraints)
    end)
  end

  defp domino_weight({r, c, pr, pc}, my_constraints) do
    w =
      (if(Enum.at(my_constraints.left, r) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.right, r) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.left, pr) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.right, pr) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.top, c) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.bottom, c) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.top, pc) != -1, do: 1, else: 0)) +
        (if(Enum.at(my_constraints.bottom, pc) != -1, do: 1, else: 0))
    w
  end

  # Recursively process the list of dominoes.
  defp solve_dominoes(my_state, my_dominoes, index, num_rows, num_cols, my_constraints, my_orientation) do
    if index == length(my_dominoes) do
      if final_constraints_ok?(my_state, num_rows, num_cols, my_constraints) do
        my_state
      else
        :error
      end
    else
      {r, c, pr, pc} = Enum.at(my_dominoes, index)

      if get_cell(my_state.board, r, c) != nil or get_cell(my_state.board, pr, pc) != nil do
        solve_dominoes(my_state, my_dominoes, index + 1, num_rows, num_cols, my_constraints, my_orientation)
      else
        # Try assignments in this order: active {"+", "-"}, active {"-", "+"}, then empty {"X", "X"}.
        options = [
          {"+", "-"},
          {"-", "+"},
          {"X", "X"}
        ]
        try_options(options, my_state, my_dominoes, index, num_rows, num_cols, my_constraints, {r, c, pr, pc}, my_orientation)
      end
    end
  end

  defp try_options([], _my_state, _my_dominoes, _index, _num_rows, _num_cols, _my_constraints, _domino, _my_orientation),
    do: :error

  defp try_options([opt | opts], my_state, my_dominoes, index, num_rows, num_cols, my_constraints, {r, c, pr, pc} = my_domino, my_orientation) do
    case try_assignment(my_state, my_constraints, r, c, pr, pc, opt, num_rows, num_cols) do
      {:ok, new_state} ->
        case solve_dominoes(new_state, my_dominoes, index + 1, num_rows, num_cols, my_constraints, my_orientation) do
          :error ->
            try_options(opts, my_state, my_dominoes, index, num_rows, num_cols, my_constraints, my_domino, my_orientation)
          sol ->
            sol
        end

      :error ->
        try_options(opts, my_state, my_dominoes, index, num_rows, num_cols, my_constraints, my_domino, my_orientation)
    end
  end

  # Attempt to assign a domino with the given option.
  defp try_assignment(my_state, my_constraints, r, c, pr, pc, {val1, val2}, num_rows, num_cols) do
    new_state =
      my_state
      |> update_state(r, c, val1, num_cols)
      |> update_state(pr, pc, val2, num_cols)

    if valid_neighbors?(new_state.board, r, c, val1, {pr, pc}, num_rows, num_cols) and
         valid_neighbors?(new_state.board, pr, pc, val2, {r, c}, num_rows, num_cols) and
         partial_constraints_ok?(new_state, num_rows, num_cols, my_constraints) do
      {:ok, new_state}
    else
      :error
    end
  end

  # Update the state at cell (r, c) with a given value and update the counters.
  defp update_state(my_state, r, c, value, _num_cols) do
    new_board = set_cell(my_state.board, r, c, value)
    {delta_plus, delta_minus} =
      case value do
        "+" -> {1, 0}
        "-" -> {0, 1}
        "X" -> {0, 0}
        _   -> {0, 0}
      end

    new_row_plus = List.update_at(my_state.row_plus, r, fn count -> count + delta_plus end)
    new_row_minus = List.update_at(my_state.row_minus, r, fn count -> count + delta_minus end)
    new_col_plus = List.update_at(my_state.col_plus, c, fn count -> count + delta_plus end)
    new_col_minus = List.update_at(my_state.col_minus, c, fn count -> count + delta_minus end)
    %{my_state | board: new_board, row_plus: new_row_plus, row_minus: new_row_minus,
      col_plus: new_col_plus, col_minus: new_col_minus}
  end

  # Check that the newly assigned cell at (r, c) has no adjacent (non-"X") neighbor with the same polarity.
  defp valid_neighbors?(the_board, r, c, value, partner_coord, num_rows, num_cols) do
    if value in ["X", nil] do
      true
    else
      for {dr, dc} <- [{-1, 0}, {1, 0}, {0, -1}, {0, 1}],
          nr = r + dr,
          nc = c + dc,
          nr >= 0, nr < num_rows,
          nc >= 0, nc < num_cols do
        if {nr, nc} == partner_coord do
          true
        else
          case get_cell(the_board, nr, nc) do
            nil -> true
            "X" -> true
            other -> other != value
          end
        end
      end
      |> Enum.all?(& &1)
    end
  end

  # Check partial constraints using the incremental counters.
  defp partial_constraints_ok?(my_state, num_rows, num_cols, my_constraints) do
    row_ok =
      Enum.all?(0..(num_rows - 1), fn r ->
        total = num_cols
        assigned = count_assigned(Enum.at(my_state.board, r))
        unassigned = total - assigned
        left_req = Enum.at(my_constraints.left, r)
        right_req = Enum.at(my_constraints.right, r)
        (left_req == -1 or (Enum.at(my_state.row_plus, r) <= left_req and Enum.at(my_state.row_plus, r) + unassigned >= left_req)) and
          (right_req == -1 or (Enum.at(my_state.row_minus, r) <= right_req and Enum.at(my_state.row_minus, r) + unassigned >= right_req))
      end)

    col_ok =
      Enum.all?(0..(num_cols - 1), fn c ->
        total = num_rows
        assigned = count_assigned_col(my_state.board, c)
        unassigned = total - assigned
        top_req = Enum.at(my_constraints.top, c)
        bottom_req = Enum.at(my_constraints.bottom, c)
        (top_req == -1 or (Enum.at(my_state.col_plus, c) <= top_req and Enum.at(my_state.col_plus, c) + unassigned >= top_req)) and
          (bottom_req == -1 or (Enum.at(my_state.col_minus, c) <= bottom_req and Enum.at(my_state.col_minus, c) + unassigned >= bottom_req))
      end)

    row_ok and col_ok
  end

  # Check that final constraints hold exactly: all cells are assigned and the counts match.
  defp final_constraints_ok?(my_state, num_rows, num_cols, my_constraints) do
    row_ok =
      Enum.all?(0..(num_rows - 1), fn r ->
        count_assigned(Enum.at(my_state.board, r)) == num_cols and
          (Enum.at(my_constraints.left, r) == -1 or Enum.at(my_state.row_plus, r) == Enum.at(my_constraints.left, r)) and
          (Enum.at(my_constraints.right, r) == -1 or Enum.at(my_state.row_minus, r) == Enum.at(my_constraints.right, r))
      end)

    col_ok =
      Enum.all?(0..(num_cols - 1), fn c ->
        count_assigned_col(my_state.board, c) == num_rows and
          (Enum.at(my_constraints.top, c) == -1 or Enum.at(my_state.col_plus, c) == Enum.at(my_constraints.top, c)) and
          (Enum.at(my_constraints.bottom, c) == -1 or Enum.at(my_state.col_minus, c) == Enum.at(my_constraints.bottom, c))
      end)

    row_ok and col_ok
  end

  defp count_assigned(row) do
    Enum.count(row, fn cell -> cell != nil end)
  end

  defp count_assigned_col(the_board, c) do
    Enum.count(the_board, fn row -> Enum.at(row, c) != nil end)
  end

  # Safely get the value at (r, c) from a board.
  defp get_cell(the_board, r, c) do
    if r < 0 or c < 0 or r >= length(the_board) or c >= length(hd(the_board)) do
      nil
    else
      Enum.at(Enum.at(the_board, r), c)
    end
  end

  # Update a cell at (r, c) with a given value.
  defp set_cell(the_board, r, c, value) do
    List.update_at(the_board, r, fn row ->
      List.update_at(row, c, fn _ -> value end)
    end)
  end

  # Convert the assignment board (a 2D list) into the expected tuple of strings.
  # Any cell still nil becomes "X".
  defp board_to_tuple(the_board) do
    the_board
    |> Enum.map(fn row ->
         row
         |> Enum.map(fn cell -> if cell == nil, do: "X", else: cell end)
         |> Enum.join("")
       end)
    |> List.to_tuple()
  end

  # If no solution is found, return a board (tuple of strings) filled with "X".
  defp empty_solution(input_board) do
    Tuple.to_list(input_board)
    |> Enum.map(fn row -> String.duplicate("X", String.length(row)) end)
    |> List.to_tuple()
  end
end
