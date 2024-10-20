defmodule AdventOfCode.Y2022.Day24Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2224

  alias AdventOfCode.Y2022.Day24, as: Solution

  @tag :skip_slow
  test "Year 2022, Day 24 run/1" do
    assert Solution.run() == {332, 942}
  end
end
