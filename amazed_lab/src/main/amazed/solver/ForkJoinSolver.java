package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;


/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    private final List<ForkJoinSolver> solvers = new ArrayList<>();
    private static final Set<Integer> visitedNodes = new ConcurrentSkipListSet<>();


    private int player;
    private static boolean goalFound = false;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        this.player = maze.newPlayer(start);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    private ForkJoinSolver(Maze maze,int newPlayer, int startNode, int forkAfter)
    {
        super(maze);
        this.forkAfter = forkAfter;
        this.player = newPlayer;
        this.start = startNode;
    }

    private ForkJoinSolver(Maze maze, int newPlayer, int startNode){
        super(maze);
        this.player = newPlayer;
        this.start = startNode;
    }



    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {

        // start with start node
        frontier.push(start);
        // mark the start node as visited
        visitedNodes.add(start);
        // as long as not all nodes have been processed and the goal has not been found
        while (!frontier.empty() && !goalFound) {

            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                goalFound = true;
                return pathFromTo(start, current);
            }

            // convert set of neighbors into a list
            List<Integer> nbs = new ArrayList<>(maze.neighbors(current));
            // boolean to make sure we only skip once
            boolean skipped = false;
            //move player to the current node
            maze.move(player, current);

            // for every neighbor
            for(int i = 0; i < nbs.size(); i++){
                int nb = nbs.get(i);
                // if neighbour was not visited, we add it as visited and continue
                if(visitedNodes.add(nb)){
                    // we push one neighbour to the frontier for the spawning process to
                    // continue working on
                    if(!skipped){
                        frontier.push(nb);
                        skipped = true;
                    }
                    //All other neighbors will be processed by new threads
                    else {
                        //create a new player to for the neighbor node
                        int newPlayer = maze.newPlayer(nb);
                        ForkJoinSolver solver = new ForkJoinSolver(maze, newPlayer, nb);
                        // add the solver to the list of solvers
                        solvers.add(solver);
                        // fork the solver into new workers
                        solver.fork();
                    }
                    // Keeps track of what node came before the other
                    predecessor.put(nb, current);
                }
            }
        }

        // Loop joining all forks together
        for (ForkJoinSolver solver : solvers){
            // Joins back a solver
            List<Integer> path = solver.join();

            // If we got a path
            if (path != null) {
                // Remove first position in path (Otherwise it will be duplicate)
                int solverStartPos = path.remove(0);
                // Create a path from this workers startPosition to the solver start position
                List<Integer> currentPath = pathFromTo(start, solverStartPos);

                // Add the solverpath and own path together
                currentPath.addAll(path);
                // Return this path
                return currentPath;
            }
        }
        // all nodes explored, no goal found
        return null;
    }

    /*
    private List<Integer> parallelSearchWait() {

        // start with start node
        frontier.push(start);
        // mark the start node as visited
        visitedNodes.add(start);
        // as long as not all nodes have been processed and the goal has not been found
        while (!frontier.empty() && !goalFound) {

            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                goalFound = true;
                return pathFromTo(start, current);
            }


            // convert set of neighbors into a list
            List<Integer> nbs = new ArrayList<>(maze.neighbors(current));
            // boolean to make sure we only skip once

            //move player to the current node
            maze.move(player, current);

            // for every neighbor
            for(int i = 0; i < nbs.size(); i++){
                int nb = nbs.get(i);


                // if neighbour was not visited, we add it as visited and continue
                if(visitedNodes.add(nb)) {
                    if(nbs.size() == 2){
                        frontier.push(nb);
                    }
                    else{
                        // we push one neighbour to the frontier for the spawning process to
                        // continue working on
                        //All other neighbors will be processed by new threads
                        //create a new player to for the neighbor node
                        int newPlayer = maze.newPlayer(nb);
                        ForkJoinSolver solver = new ForkJoinSolver(maze, newPlayer, nb);
                        // add the solver to the list of solvers
                        solvers.add(solver);
                        // fork the solver into new workers
                        solver.fork();
                        // Keeps track of what node came before the other

                    }
                    predecessor.put(nb, current);

                }

            }

            // Loop joining all forks together
            for (ForkJoinSolver solver : solvers){
                // Joins back a solver
                List<Integer> path = solver.join();

                // If we got a path
                if (path != null) {
                    // Remove first position in path (Otherwise it will be duplicate)
                    int solverStartPos = path.remove(0);
                    // Create a path from this workers startPosition to the solver start position
                    List<Integer> currentPath = pathFromTo(start, solverStartPos);

                    // Add the solverpath and own path together
                    currentPath.addAll(path);
                    // Return this path
                    return currentPath;
                }
            }
        }


        // all nodes explored, no goal found
        return null;
    }

     */

    private List<Integer> parallelSearchForkAfter() {

        // start with start node
        frontier.push(start);
        // mark the start node as visited
        visitedNodes.add(start);

        // initialize the counter using forkAfter
        int counter = forkAfter + 1;
        // as long as not all nodes have been processed and the goal has not been found
        while (!frontier.empty() && !goalFound) {

            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                goalFound = true;
                return pathFromTo(start, current);
            }

            //move player to the current node
            maze.move(player, current);
            counter--;

            // loop through each neighbor
            for (int nb: maze.neighbors(current)) {
                if(visitedNodes.add(nb)) {
                    if (counter == 0) {
                        //create a new player for the neighbor node
                        int newPlayer = maze.newPlayer(nb);
                        ForkJoinSolver solver = new ForkJoinSolver(maze, newPlayer, nb, forkAfter);
                        // add the solver to the list of solvers
                        solvers.add(solver);
                        solver.fork();

                    } else {
                        frontier.push(nb);
                    }
                    predecessor.put(nb, current);
                }
            }

            if(counter == 0){
                counter = forkAfter;
            }
            // Loop joining all forks together
            for (ForkJoinSolver solver : solvers){
                // Joins back a solver
                List<Integer> path = solver.join();

                // If we got a path
                if (path != null) {
                    List<Integer> currentPath = pathFromTo(start, current); // Create a path from this workers startPosition to current

                    currentPath.addAll(path);   // Add the solverpath and own path together
                    return currentPath;     // Return this path
                }
            }




        }



        // all nodes explored, no goal found
        return null;
    }
}
