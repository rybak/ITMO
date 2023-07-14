package ru.ifmo.ctddev.rybak.simpleiter;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.Stack;

import javax.swing.*;

@SuppressWarnings("serial")
public class Visualizer extends JFrame implements ActionListener, KeyListener,
		MouseMotionListener, MouseListener {

	private static final double DEFAULT_MINX = -0.1;// -2.15;
	private static final double DEFAULT_MAXX = 4.15;
	private static final double DEFAULT_MAXY = 1.1;// 1.5;
	private static final double DEFAULT_MINY = -0.1;// -0.6;
	private static final double DEFAULT_X0 = 0.5;
	private static final int DEFAULT_ITER = 200;
	JPanel chart = null;

	Graphics2D graphics2D;

	JPanel bottom = null;
	JButton drawButton = null;
	JButton resetButton = null;
	JTextField iterTextField = null;
	JTextField minXTextField = null;
	JTextField maxXTextField = null;
	JTextField minYTextField = null;
	JTextField maxYTextField = null;

	JTextField x0TextField = null;
	JLabel errorLabel = null;

	JPanel top = null;
	JLabel displayedXY = null;

	public final static int HEIGHT = 500;
	public final static int WIDTH = 800;
	private Calculator calc;

	public static int BUTTOMBORDER = 150;
	public static int TOPBORDER = 50;
	public static int VSPACE = 10;
	public static int LEFTBORDER = 0;
	public static int RIGHTBORDER = 0;

	private BufferedImage buf = null;

	public Visualizer() {
		stack = new Stack<Pair>();
		setTitle("Simple Iterations");

		top = new JPanel();
		chart = new JPanel();
		bottom = new JPanel();
		setLocation(20, 20);
		setSize(LEFTBORDER + WIDTH + RIGHTBORDER, TOPBORDER + HEIGHT
				+ BUTTOMBORDER + 2 * VSPACE);
		buildTop();
		buildChart();
		buildBottom();

		getContentPane().setLayout(null);
		getContentPane().add(top);
		getContentPane().add(chart, -1);
		getContentPane().add(bottom, -1);

		buildCalc();

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		RepaintManager.setCurrentManager(new RepaintManager() {
			public void paintDirtyRegions() {
				super.paintDirtyRegions();
			}
		});
		setVisible(true);
		graphics2D = (Graphics2D) chart.getGraphics();
		buf = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB);
	}

	private void buildCalc() {
		calc = new Calculator(WIDTH, HEIGHT, getMinX(), getMaxX(), getMinY(),
				getMaxY(), getX0(), getIter());
	}

	private int getIter() {
		return getIntFromTextField(iterTextField, DEFAULT_ITER);
	}

	private int getIntFromTextField(JTextField tf, int defaultValue) {
		try {
			int a = Integer.parseInt(tf.getText());
			errorLabel.setVisible(false);
			return a;
		} catch (NumberFormatException e) {
			errorLabel.setVisible(true);
			return defaultValue;
		}
	}

	private double getX0() {
		return getDoubleFromTextField(x0TextField, DEFAULT_X0);
	}

	private double getMinX() {
		return getDoubleFromTextField(minXTextField, DEFAULT_MINX);
	}

	private double getMaxX() {
		return getDoubleFromTextField(maxXTextField, DEFAULT_MAXX);
	}

	private double getMinY() {
		return getDoubleFromTextField(minYTextField, DEFAULT_MINY);
	}

	private double getMaxY() {
		return getDoubleFromTextField(maxYTextField, DEFAULT_MAXY);
	}

	private double getDoubleFromTextField(JTextField tf, double defaultValue) {
		try {
			double a = Double.parseDouble(tf.getText());
			errorLabel.setVisible(false);
			return a;
		} catch (NumberFormatException e) {
			errorLabel.setVisible(true);
			return defaultValue;
		}
	}

	private void draw() {
		draw(getGraphics2D());
	}

	public Graphics2D getGraphics2D() {
		return graphics2D;
	}

	public void draw(Graphics2D g) {
		Graphics2D bufg = (Graphics2D) buf.getGraphics();
		clean(bufg);
		buildCalc();
		calc.draw(bufg);
		g.drawImage(buf, 0, 0, WIDTH, HEIGHT, null);
	}

	private void clean() {
		clean(getGraphics2D());
	}

	private void clean(Graphics2D g) {
		g.setColor(chart.getBackground());
		g.clearRect(0, 0, WIDTH, HEIGHT);
		g.fillRect(0, 0, WIDTH, HEIGHT);
	}

	public void error(String string) {
		JOptionPane.showMessageDialog(null, string, "Error",
				JOptionPane.ERROR_MESSAGE);
	}

	private static final int topLabelX = 5;
	private static final int topLabelY = 5;
	private static final int topLabelWidth = 50;
	private static final int topLabelHeight = 20;
	private static final int topLabelWidth2 = 200;

	private void buildTop() {
		top.setLayout(null);
		JLabel xyLabel = new JLabel("(x,y) = ");
		xyLabel.setBounds(topLabelX, topLabelY, topLabelWidth, topLabelHeight);
		displayedXY = new JLabel("(0, 0)");
		displayedXY.setBounds(topLabelX + topLabelWidth, topLabelY,
				topLabelWidth2, topLabelHeight);
		top.add(displayedXY);
		top.add(xyLabel);
		top.setSize(LEFTBORDER + WIDTH + RIGHTBORDER, TOPBORDER);
		top.setLocation(LEFTBORDER, 0);
	}

	private void buildChart() {
		chart.setLayout(null);
		chart.addMouseMotionListener(this);
		chart.addMouseListener(this);
		chart.setBackground(Color.WHITE);
		chart.setLocation(LEFTBORDER, TOPBORDER + VSPACE);
		chart.setSize(WIDTH, HEIGHT);
	}

	private static final int leftButtonX = 20;
	private static final int topButtonY = 2;
	private static final int buttonXSpace = 20;
	private static final int buttonYSpace = 5;

	private static final int buttonWidth = 100;
	private static final int buttonHeight = 20;

	private void buildBottom() {
		drawButton = new JButton("<html><body>Draw</body></html>");
		setBoundsOnBottom(drawButton, 0, 0, 1);
		drawButton.addActionListener(this);
		drawButton.addKeyListener(this);

		resetButton = new JButton("<html><body>Reset</body></html>");
		setBoundsOnBottom(resetButton, 1, 0, 1);
		resetButton.addActionListener(this);
		resetButton.addKeyListener(this);

		maxYTextField = buildTextField(DEFAULT_MAXY, 3, 0, "max y");
		minXTextField = buildTextField(DEFAULT_MINX, 2, 1, "min x");
		maxXTextField = buildTextField(DEFAULT_MAXX, 4, 1, "max x");
		minYTextField = buildTextField(DEFAULT_MINY, 3, 2, "min y");
		x0TextField = buildTextField(DEFAULT_X0, 0, 1, "x0");
		iterTextField = buildTextField(DEFAULT_ITER, 0, 2, "iterations");

		errorLabel = new JLabel("Wrong input format");
		setBoundsOnBottom(errorLabel, 2, 3, 2);

		bottom.setLayout(null);
		bottom.setSize(LEFTBORDER + WIDTH + RIGHTBORDER, BUTTOMBORDER);
		bottom.setLocation(LEFTBORDER, chart.getY() + chart.getHeight()
				+ VSPACE);
		bottom.add(drawButton);
		bottom.add(resetButton);
		bottom.add(minXTextField);
		bottom.add(maxXTextField);
		bottom.add(minYTextField);
		bottom.add(maxYTextField);
		bottom.add(iterTextField);
		bottom.add(x0TextField);
		bottom.add(errorLabel);
	}

	private void setBoundsOnBottom(JComponent c, int x, int y, int w) {
		c.setBounds(leftButtonX + x * (buttonWidth + buttonXSpace), topButtonY
				+ y * (buttonHeight + buttonYSpace), w * buttonWidth,
				buttonHeight);
	}

	private JTextField buildTextField(Number defaultValue, int x, int y,
			String toolTip) {
		JTextField textField = new JTextField(defaultValue.toString());
		textField.setToolTipText(toolTip);
		setBoundsOnBottom(textField, x, y, 1);

		textField.addActionListener(this);
		textField.addKeyListener(this);
		return textField;
	}

	public void paint(Graphics g) {
		super.paint(g);
		draw();
	}

	public void update(Graphics g) {
		super.update(g);
	}

	public void repaint() {
		super.repaint();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
	}

	public static final double SHOW_DIGITS = 1000.0;

	@Override
	public void mouseMoved(MouseEvent e) {
		updateDisplayedXY(e.getX(), e.getY());
	}

	public static String showDouble(double a) {
		return Double.toString(Math.round(a * SHOW_DIGITS) / SHOW_DIGITS);
	}

	private void updateDisplayedXY(int xx, int yy) {
		String x = showDouble(calc.getRealX(xx));
		String y = showDouble(calc.getRealY(yy));
		displayedXY.setVisible(false);
		displayedXY.setText("(" + x + ", " + y + ")");
		displayedXY.setVisible(true);
	}

	@Override
	public void keyTyped(KeyEvent e) {
	}

	@Override
	public void keyPressed(KeyEvent e) {
	}

	@Override
	public void keyReleased(KeyEvent e) {
	}

	private void reset() {
		minXTextField.setText(Double.toString(DEFAULT_MINX));
		maxXTextField.setText(Double.toString(DEFAULT_MAXX));
		minYTextField.setText(Double.toString(DEFAULT_MINY));
		maxYTextField.setText(Double.toString(DEFAULT_MAXY));
		iterTextField.setText(Integer.toString(DEFAULT_ITER));
		x0TextField.setText(Double.toString(DEFAULT_X0));
		clean();
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		Object obj = event.getSource();
		if (obj == drawButton) {
			buildCalc();
			draw();
			return;
		}
		if (obj == resetButton) {
			reset();
			return;
		}
	}

	public void init() {
	}

	boolean first = true;

	class Pair {
		double x, y;

		Pair(double x, double y) {
			this.x = x;
			this.y = y;
		}

		boolean equals(Pair p) {
			return p.x == x && p.y == y;
		}
	}

	Stack<Pair> stack;

	@Override
	public void mouseClicked(MouseEvent e) {
		String x = showDouble(calc.getRealX(e.getX()));
		String y = showDouble(calc.getRealY(e.getY()));
		if (first) {
			minXTextField.setText(x);
			minYTextField.setText(y);
		} else {
			maxXTextField.setText(x);
			maxYTextField.setText(y);
		}
		first = !first;
	}

	@Override
	public void mousePressed(MouseEvent e) {

	}

	@Override
	public void mouseReleased(MouseEvent e) {

	}

	@Override
	public void mouseEntered(MouseEvent e) {

	}

	@Override
	public void mouseExited(MouseEvent e) {

	}

}
