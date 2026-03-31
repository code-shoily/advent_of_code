defmodule AdventOfCode.Y2024.Day18Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2418

  alias AdventOfCode.Y2024.Day18, as: Solution

  test "Year 2024, Day 18 run/1" do
    assert Solution.run() == {226, "60,46"}
  end
end
