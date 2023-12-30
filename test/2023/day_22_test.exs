defmodule AdventOfCode.Y2023.Day22Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2322

  alias AdventOfCode.Y2023.Day22, as: Solution

  test "Year 2023, Day 22 run/1" do
    assert Solution.run() == {490, 96_356}
  end
end
