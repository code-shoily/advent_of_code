defmodule AdventOfCode.Y2020.Day23Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2023

  alias AdventOfCode.Y2020.Day23, as: Solution

  test "Year 2020, Day 23" do
    assert Solution.run() == {"43769582", 264_692_662_390}
  end
end
