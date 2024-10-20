defmodule AdventOfCode.Y2015.Day09Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1509

  alias AdventOfCode.Y2015.Day09, as: Solution

  test "Year 2015, Day 9" do
    assert Solution.run() == {117, 909}
  end
end
