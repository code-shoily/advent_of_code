defmodule AdventOfCode.Y2022.Day15Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2215

  alias AdventOfCode.Y2022.Day15, as: Solution

  test "Year 2022, Day 15 run/1" do
    assert Solution.run() == {4_725_496, 12_051_287_042_458}
  end
end
