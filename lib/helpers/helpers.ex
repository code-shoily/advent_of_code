defmodule AdventOfCode.Transformers do
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
