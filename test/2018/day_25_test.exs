defmodule AdventOfCode.Y2018.Day25Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1825

  alias AdventOfCode.Y2018.Day25, as: Solution

  test "Year 2018, Day 25 run/1" do
    assert Solution.run() == {346, "Finished the 4D adventure! 🥳"}
  end
end
