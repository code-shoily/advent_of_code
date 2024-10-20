defmodule AdventOfCode.Y2020.Day08Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2008

  alias AdventOfCode.Y2020.Day08, as: Solution

  test "Year 2020, Day 8" do
    assert Solution.run() == {2080, 2477}
  end
end
