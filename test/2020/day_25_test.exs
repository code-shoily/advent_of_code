defmodule AdventOfCode.Y2020.Day25Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2025

  alias AdventOfCode.Y2020.Day25, as: Solution

  test "Year 2020, Day 25" do
    assert Solution.run() == {3_286_137, "🎉"}
  end
end
