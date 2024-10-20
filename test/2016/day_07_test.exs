defmodule AdventOfCode.Y2016.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1607

  alias AdventOfCode.Y2016.Day07, as: Solution

  test "Year 2016, Day 7" do
    assert Solution.run() == {105, 258}
  end
end
