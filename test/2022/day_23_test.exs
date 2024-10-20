defmodule AdventOfCode.Y2022.Day23Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2223

  alias AdventOfCode.Y2022.Day23, as: Solution

  @tag :skip_slow
  test "Year 2022, Day 23 run/1" do
    assert Solution.run() == {4116, 984}
  end
end
