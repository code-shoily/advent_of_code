defmodule AdventOfCode.Y2023.Day25Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2325

  alias AdventOfCode.Y2023.Day25, as: Solution

  @tag :skip_slow
  test "Year 2023, Day 25 run/1" do
    assert Solution.run() == {558_376, "🎉"}
  end
end
