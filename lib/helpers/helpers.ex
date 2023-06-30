defmodule AdventOfCode.Helpers do
  @moduledoc """
  Various helper functions during development and testing.
  """
  @type board :: {non_neg_integer(), any()}
  @type elem :: any()
  @type idx :: non_neg_integer()
  @type list_2d :: list(list(elem()))

  @doc """
  Prints a 2D board of the format map(tuple(idx, val)).
  """
  @spec print2d(%{required(idx()) => board()}) :: list_2d()
  def print2d(map) do
    {{min_x, min_y}, {max_x, max_y}} = map |> Map.keys() |> Enum.min_max()

    for i <- min_x..max_x do
      for j <- min_y..max_y do
        map[{i, j}]
      end
    end
  end

  @doc """
  Prints a 2D grid in stdout. No value is returned.
  """
  @spec print2d(%{required(idx()) => board()}) :: no_return()
  def print2d_io(grid) do
    {x, y} = grid |> Map.keys() |> Enum.max()

    for i <- 0..x do
      for j <- 0..y do
        IO.write(grid[{i, j}])
      end

      IO.puts("")
    end
  end
end
