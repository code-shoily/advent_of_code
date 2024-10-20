defmodule AdventOfCode.Y2023.Day23Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2323

  alias AdventOfCode.Y2023.Day23, as: Solution

  @tag :skip_slow
  test "Year 2023, Day 23 run/1" do
    assert Solution.run() == {2042, 6466}
  end
end
