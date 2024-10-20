defmodule AdventOfCode.Y2015.Day14Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1514

  alias AdventOfCode.Y2015.Day14, as: Solution

  test "Year 2015, Day 14" do
    assert Solution.run() == {2640, 1102}
  end
end
