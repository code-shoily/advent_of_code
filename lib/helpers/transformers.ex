defmodule AdventOfCode.Helpers.Transformers do
  @moduledoc """
  Module to help transform data structures
  """

  @doc """
  Converts a list of list into a map of map.

  ## Example

    iex> Transformers.grid2d([])
    %{}

    iex> Transformers.grid2d([[1, 3, 5], [2, 4, 6]])
    %{
      {0, 0} => 1,
      {0, 1} => 3,
      {0, 2} => 5,
      {1, 0} => 2,
      {1, 1} => 4,
      {1, 2} => 6
    }

    iex> Transformers.grid2d([[1], [2, 3]], fn i -> i**3 end)
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
  Transforms a 2x2 matrix

  ## Example

      iex> Transformers.transpose([[]])
      []

      iex> Transformers.transpose([])
      []

      iex> Transformers.transpose([[1, 2], [3, 4]])
      [[1, 3], [2, 4]]

      iex> Transformers.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
      [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

  """
  def transpose(matrix) do
    matrix
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
