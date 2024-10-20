defmodule AdventOfCode.Y2022.Day25Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2225

  alias AdventOfCode.Y2022.Day25, as: Solution

  test "Year 2022, Day 25 run/1" do
    assert Solution.run() == {"2-==10===-12=2-1=-=0", "🎉"}
  end
end
