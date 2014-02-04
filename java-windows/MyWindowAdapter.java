import java.awt.event.*;
import java.awt.*;
public class MyWindowAdapter extends WindowAdapter
{ 
public void windowClosing (WindowEvent we)
{
we.getWindow().dispose();
}
    /*public void textChanged (TextEvent te)
    {
	te.getWindow().show();
    }*/
}
