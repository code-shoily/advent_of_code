defmodule AdventOfCode.Y2024.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2407

  alias AdventOfCode.Y2024.Day07, as: Solution

  test "Year 2024, Day 7 run/0" do
    assert Solution.run() == {882_304_362_421, 145_149_066_755_184}
  end
end
