defmodule AdventOfCode.Helpers.Combinatorics do
  @doc """
  Computes the permutations of a list.

  ## Example

      iex> Combinatorics.permutations([])
      [[]]

      iex> Combinatorics.permutations([1, 2])
      [[1, 2], [2, 1]]

      iex> Combinatorics.permutations([1, 2, 3])
      [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

  """
  def permutations([]) do
    [[]]
  end

  def permutations(list) do
    for h <- list, t <- permutations(list -- [h]), do: [h | t]
  end
end
