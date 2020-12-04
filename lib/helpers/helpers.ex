defmodule AdventOfCode.Transformers do
  @doc """
  Converts a list of list into a map of map.

  ## Example

    iex> AdventOfCode.Transformers.grid2d([])
    %{}

    iex> AdventOfCode.Transformers.grid2d([[1, 3, 5], [2, 4, 6]])
    %{0 => %{0 => 1, 1 => 3, 2 => 5}, 1 => %{0 => 2, 1 => 4, 2 => 6}}

    iex> AdventOfCode.Transformers.grid2d([[1], [2, 3]])
    %{0 => %{0 => 1}, 1 => %{0 => 2, 1 => 3}}

  """
  def grid2d(data) do
    for {vx, ix} <- Enum.with_index(data),
        into: %{} do
      {ix,
       for {vy, iy} <- Enum.with_index(vx),
           into: %{} do
         {iy, vy}
       end}
    end
  end
end
