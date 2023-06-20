defmodule AdventOfCode.Y2017.Day23Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1723

  alias AdventOfCode.Y2017.Day23, as: Solution

  test "Year 2017, Day 23 run/1" do
    assert Solution.run() == {6241, {:todo, 2}}
  end
end
