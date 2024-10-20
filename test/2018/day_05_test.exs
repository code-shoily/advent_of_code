defmodule AdventOfCode.Y2018.Day05Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1805

  alias AdventOfCode.Y2018.Day05, as: Solution

  test "React (1)" do
    assert Solution.react(~c"dabAcCaCBAcCcaDA") == 10
  end

  test "Year 2018, Day 5" do
    assert Solution.run() == {10_496, 5774}
  end
end
