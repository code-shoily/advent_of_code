defmodule AdventOfCode.Algorithms.Grid do
  @moduledoc """
  Algorithms involving grid or 2D data
  """

  @doc """
  Converts a list of list into a map with keys as tuple and mappable value.

  ## Example

      iex> Grid.grid2d([])
      %{}

      iex> Grid.grid2d([[1, 3, 5], [2, 4, 6]])
      %{
        {0, 0} => 1,
        {0, 1} => 3,
        {0, 2} => 5,
        {1, 0} => 2,
        {1, 1} => 4,
        {1, 2} => 6
      }

      iex> Grid.grid2d([[1], [2, 3]], fn i -> i**3 end)
      %{
        {0, 0} => 1,
        {1, 0} => 8,
        {1, 1} => 27
      }

  """
  def grid2d(data, tx \\ &Function.identity/1) do
    for {row, row_idx} <- Enum.with_index(data),
        {cell, col_idx} <- Enum.with_index(row),
        into: %{} do
      {{row_idx, col_idx}, tx.(cell)}
    end
  end

  @doc """
  Gets the co-ordinates of 8 surrounding neighbours in a 2D grid.

  ## Example

      iex> Grid.surrounding8({0, 0})
      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {1, 1}, {-1, -1}, {1, -1}, {-1, 1}]

  """
  def surrounding8({x, y}) do
    Enum.map(
      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {1, 1}, {-1, -1}, {1, -1}, {-1, 1}],
      fn {dx, dy} -> {x + dx, y + dy} end
    )
  end
end
