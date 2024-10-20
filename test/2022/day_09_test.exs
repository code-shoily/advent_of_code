defmodule AdventOfCode.Y2022.Day09Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2209

  alias AdventOfCode.Y2022.Day09, as: Solution

  test "Year 2022, Day 9 run/1" do
    assert Solution.run() == {5907, 2303}
  end
end
