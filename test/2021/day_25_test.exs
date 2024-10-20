defmodule AdventOfCode.Y2021.Day25Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2125

  alias AdventOfCode.Y2021.Day25, as: Solution

  test "Year 2021, Day 25 run/1" do
    assert Solution.run() == {504, "🎉"}
  end
end
