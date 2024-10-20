defmodule AdventOfCode.Y2015.Day12Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1512

  alias AdventOfCode.Y2015.Day12, as: Solution

  test "Year 2015, Day 12" do
    assert Solution.run() == {119_433, 68_466}
  end
end
