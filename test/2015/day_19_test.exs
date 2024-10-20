defmodule AdventOfCode.Y2015.Day19Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1519

  alias AdventOfCode.Y2015.Day19, as: Solution

  test "Year 2015, Day 19 run/1" do
    assert Solution.run() == {535, 212}
  end
end
