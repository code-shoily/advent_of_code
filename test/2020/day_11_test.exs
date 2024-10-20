defmodule AdventOfCode.Y2020.Day11Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2011

  alias AdventOfCode.Y2020.Day11, as: Solution

  test "Year 2020, Day 11" do
    assert Solution.run() == {2427, 2199}
  end
end
