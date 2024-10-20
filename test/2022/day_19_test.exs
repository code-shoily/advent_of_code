defmodule AdventOfCode.Y2022.Day19Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2219

  alias AdventOfCode.Y2022.Day19, as: Solution

  @tag :skip_slow
  test "Year 2022, Day 19 run/1" do
    assert Solution.run() == {1599, 14_112}
  end
end
