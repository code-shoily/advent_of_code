defmodule AdventOfCode.Y2016.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1617

  alias AdventOfCode.Y2016.Day17, as: Solution

  test "Year 2016, Day 17 run/1" do
    assert Solution.run() == {"DURLDRRDRD", 650}
  end
end
