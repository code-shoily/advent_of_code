defmodule AdventOfCodeTest do
  use ExUnit.Case
  doctest AdventOfCode

  test "greets the world" do
    assert AdventOfCode.hello() == :world
  end
end
