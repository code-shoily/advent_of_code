defmodule AdventOfCode.Y2022.Day11Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2211

  alias AdventOfCode.Y2022.Day11, as: Solution

  test "Year 2022, Day 11 run/1" do
    assert Solution.run() == {78_678, 15_333_249_714}
  end
end
