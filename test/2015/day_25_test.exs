defmodule AdventOfCode.Y2015.Day25Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1525

  alias AdventOfCode.Y2015.Day25, as: Solution

  test "Year 2015, Day 25 run/1" do
    assert Solution.run() == {19_980_801, "🎉"}
  end
end
