defmodule AdventOfCode.Y2017.Day11Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1711

  alias AdventOfCode.Y2017.Day11, as: Solution

  test "Year 2017, Day 11 run/1" do
    assert Solution.run() == {808, 1556}
  end
end
