defmodule AdventOfCode.Algorithms.Grid do
  alias AdventOfCode.Helpers.Transformers

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

  @doc ~S"""
  Converts a nl separated block of text into a map with keys as tuple and mappable value.

  ## Example

      iex> Grid.text_to_grid2d("")
      %{}

      iex> Grid.text_to_grid2d("135\n246")
      %{
        {0, 0} => "1",
        {0, 1} => "3",
        {0, 2} => "5",
        {1, 0} => "2",
        {1, 1} => "4",
        {1, 2} => "6"
      }

      iex> Grid.text_to_grid2d("1\n2,3", &String.split(&1, ","), fn i -> String.to_integer(i)**3 end)
      %{
        {0, 0} => 1,
        {1, 0} => 8,
        {1, 1} => 27
      }

  """
  def text_to_grid2d(data, mapper \\ &String.graphemes/1, tx \\ &Function.identity/1) do
    data
    |> Transformers.lines()
    |> Enum.map(mapper)
    |> grid2d(tx)
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
      &add(&1, {x, y})
    )
  end

  @doc """
  Gets the co-ordinates of 4 surrounding neighbours in a 2D grid.

  ## Example

      iex> Grid.surrounding4({0, 0})
      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

  """
  def surrounding4({x, y}) do
    Enum.map(
      [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
      &add(&1, {x, y})
    )
  end

  @doc """
  Adds two tuples. This is useful when moving directions.

  ## Example

      iex> Grid.add({-10, -20}, {10, 20})
      {0, 0}

  """
  def add({row, col}, {move_row, move_col}) do
    {row + move_row, col + move_col}
  end
end
