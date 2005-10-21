package ModelInterface.PPsource;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class FlatIndex implements DataIndex
{
  private DataRepository data; //this is where all data is stored and where we will get it from
  private double minX;
  private double maxX;
  private double minY;
  private double maxY;
  private boolean init;
  private TreeMap makeRegion;
  
  public double resolution; //resolution of the data this index points to
  
//*********************************************************
//*****************Class Constructors**********************
//*********************************************************
  public FlatIndex()
  {
    //assumes we are using a globe
    minX = -180;
    maxX = 180;
    minY = -90;
    maxY = 90;
    resolution = -1;
  }
  
  public FlatIndex(double x1, double x2, double y1, double y2)
  {
    //dont really need this...
    minX = x1;
    maxX = x2;
    minY = y1;
    maxY = y2;
    resolution = -1;
  }
  
//*********************************************************
//*************Begin Functions Proper**********************
//*********************************************************
  
  public double getResolution()
  {
    return resolution;
  }

  public void fillWorld(double res)
  {
    //doesnt actually fill anything as this is a representationless index
    //just sets resolution and moves on
    init(res);
  }

  public void addData(DataBlock val)
  {
    addData(val, true);
  }

  public void addData(DataBlock val, boolean avg)
  {
    Iterator i1, i2;
    Map.Entry vEntry, tEntry;
    Block indArea;
    Point min, max;
    double weight;
    
    if(!init)
    {
      init(val.width);
    }
    
    
    //find index bounds to work in
    min = point2index(new Point(val.x, val.y), true);
    max = point2index(new Point((val.x+val.width), (val.y+val.height)), false);
    
    //X, and Y are now indicies
    for(int Y = (int)min.y; Y < max.y; Y++)
    {
      for(int X = (int)min.x; X < max.x; X++)
      {
        //index area given location and resolution
        indArea = index2area(new Point(X, Y));
        
        //find the weight value
        weight = getWeight(val, indArea, avg);
        
        //pass all that to data
        if(weight > 0)
        { 
          //then there is some overlap, add data in some way
          String varName;
          double weightValue, addValue, timeName;
          
          //should add each var and time (could be more than one)
          i1 = val.data.entrySet().iterator();
          while(i1.hasNext())
          { //for each variable...
            vEntry = (Map.Entry)i1.next();
            varName = (String)vEntry.getKey();
            TreeMap thisTime = (TreeMap)vEntry.getValue();
            
            i2 = thisTime.entrySet().iterator();
            while(i2.hasNext())
            { //for each time...
              tEntry = (Map.Entry)i2.next();
              timeName = (Double)tEntry.getKey();
              addValue = (Double)tEntry.getValue();
              
              weightValue = addValue*weight;
              
              data.addValue(varName, timeName, X, Y, weightValue);
            }
            
          }
        }
        //done adding data
      }
    }
  }

  public void resolveOverwrite(String holdName, String varName)
  {
    data.mergeHoldTo(holdName, varName);
  }

  public TreeMap extractMask(RegionMask m)
  {
    makeRegion = new TreeMap(); //setting up empty data structure to add to
    Point min, max;

    //identify rectangular bounds of region
    min = point2index(new Point(m.x, m.y), true);
    max = point2index(new Point((m.x+m.width+m.resolution), (m.y+m.height+m.resolution)), false);
    
    //X, and Y are now indicies
    //for each block of info in the repository
    for(int Y = (int)min.y; Y < max.y; Y++)
    {
      for(int X = (int)min.x; X < max.x; X++)
      {
        extractBlock(new Point(X, Y), m);
      }
    }
    
    return makeRegion; //returning data
  }
  
//*********************************************************
//*************Begin Private Functions*********************
//*********************************************************
  private void init(double res)
  {
    resolution = res;
    init = true;
    data = new MatrixRepository((int)Math.floor((maxX-minX)/resolution), (int)Math.floor((maxY-minY)/resolution));
  }
  
  private Point point2index(Point p, boolean down)
  {
    //down is wether we should use floor or ceiling, wether we round down or up
    int x, y;//indexes are always ints
    
    //translate from a point in lat/lon to an index
    //possible given that we know resolution, which we do
    if(down)
    {
      x = (int)Math.floor(((p.x/resolution)+(180/resolution)));
      y = (int)Math.floor(((p.y/resolution)+(90/resolution)));
    } else
    {
      x = (int)Math.ceil(((p.x/resolution)+(180/resolution)));
      y = (int)Math.ceil(((p.y/resolution)+(90/resolution)));
    }
    
    return new Point(x, y);
  }
  private Point index2point(Point i)
  {
    double x, y;
    
    //translate from index to point
    //no need to know wether we round up or down because index is always an
    //exact translation to lat/lon
    x = ((i.x-(180/resolution))*(resolution));
    y = ((i.y-(90/resolution))*(resolution));
    
    //return this index
    return new Point(x, y);
  }
  private Block index2area(Point i)
  {
    Point p;
    //build an area given an index of said area and resolution
    //first get point corresponding to index
    p = index2point(i);
    //now return area from point
    return new Block(p.x, p.y, resolution, resolution);
  }
  private double getWeight(Block val, Block ind, boolean avg)
  {
    double weight = 0;
    double p1, p2;

    
    p1 = ind.getOverlap(val);
    p2 = val.getOverlap(ind);
    
    weight = p1;
    if(!avg)
    { //additive so have to account for limited addition of values (p2)
      weight *= p2;
    }
    
    return weight;
  }
  private void extractBlock(Point i, RegionMask m)
  {
    double weight;
    Block entry;
    TreeMap<String, TreeMap<Double, Double>> dataPoint;
    Point2D.Double addPoint;
    
    //check if part of region
    entry = index2area(i);
    weight = m.inRegion(entry.x, entry.y, entry.width, entry.height);
    
    //if so, add it
    if(weight > 0)
    {
      if(!makeRegion.containsKey("weight"))
      {
        //setting up the var and time for weight of blocks if not done yet
        TreeMap wTime = new TreeMap();
        wTime.put("0", new TreeMap(new coordComparePoint()));
        makeRegion.put("weight", wTime);
      }
      //adding a data point for the weight of this DB
      addPoint = new Point2D.Double(entry.x, entry.y);
      ((TreeMap)((TreeMap)makeRegion.get("weight")).get("0")).put(addPoint, new Double(weight));
      
      //*getting the data for this point from the data repository
      dataPoint = data.getAllLayers((int)i.x, (int)i.y);
      //*done getting data
      
      //add the data to the correct TreeMap (based on data name)
      //iterate through the data in this Node, by Var, then Time, adding to makeRegion
      Map.Entry var, time;
      Iterator iV = dataPoint.entrySet().iterator();
      while(iV.hasNext())
      {
        //iterating through variables
        var = (Map.Entry)iV.next();
        if(!makeRegion.containsKey(var.getKey()))
        {//if makeRegion does not yet have a mapping for this variable, add it now
          makeRegion.put(var.getKey(), new TreeMap());
        }
        Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
        while(iT.hasNext())
        {
          time = (Map.Entry)iT.next();
          if(!((TreeMap)makeRegion.get(var.getKey())).containsKey(time.getKey()))
          {//if makeRegion's mapping for the variable does not contain this time yet, add it now
            ((TreeMap)makeRegion.get(var.getKey())).put(time.getKey(), new TreeMap(new coordComparePoint()));
          }
          //ok we can finally add the actual data as a (point, value) pair to lowest level treeMap
          addPoint = new Point2D.Double(entry.x, entry.y);
          ((TreeMap)((TreeMap)makeRegion.get(var.getKey())).get(time.getKey())).put(addPoint, time.getValue());
        }
      }
    }
  }
  
  //this is a stupid stupid class to hold 2 numbers: for returnign and passing values
  class Point
  {
      public double x, y;
      public Point(double x, double y) 
      {
          this.x = x;
          this.y = y;
      }
  }
}