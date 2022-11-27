defmodule AdventOfCode.Y2017.Day14Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1714

  alias AdventOfCode.Y2017.Day14, as: Solution

  test "Year 2017, Day 14 run/1" do
    assert Solution.run() == {8316, {:todo, 2}}
  end
end
