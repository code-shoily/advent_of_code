defmodule AdventOfCode.Y2015.Day23Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1523

  alias AdventOfCode.Y2015.Day23, as: Solution

  test "Year 2015, Day 23 run/1" do
    assert Solution.run() == {170, 247}
  end
end
