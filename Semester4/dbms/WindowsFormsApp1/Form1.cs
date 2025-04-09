using System;
using System.Data;
using System.Data.SqlClient;
using System.Windows.Forms;

namespace WindowsFormsApp1
{
    public partial class Form1 : Form
    {
        string ConnectionString = "Data Source=172.20.10.14,1433;Initial Catalog=airbnb;Persist Security Info=True;User ID=sa;Password=<YourStrong!Passw0rd>;TrustServerCertificate=True";
        private DataSet ds = new DataSet();
        private SqlDataAdapter usersAdapter;
        private SqlDataAdapter propertiesAdapter;
        private BindingSource usersBindingSource = new BindingSource();
        private BindingSource propertiesBindingSource = new BindingSource();

        public Form1()
        {
            InitializeComponent();
            InitializeDataGridViews();
            LoadData();
        }

        private void InitializeDataGridViews()
        {
            // Users DataGridView
            dataGridViewUsers.DataSource = usersBindingSource;
            dataGridViewUsers.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            dataGridViewUsers.MultiSelect = false;
            dataGridViewUsers.SelectionChanged += DataGridViewUsers_SelectionChanged;
            dataGridViewUsers.ReadOnly = false; // Allow editing in the grid

            // Properties DataGridView
            dataGridViewProperties.DataSource = propertiesBindingSource;
            dataGridViewProperties.ReadOnly = false; // Allow editing in the grid
        }

        private void LoadData()
        {
            try
            {
                // Clear existing relations first
                if (ds.Relations.Contains("UsersProperties"))
                {
                    ds.Relations.Remove("UsersProperties");
                }

                // Load Users
                usersAdapter = new SqlDataAdapter(
                    "SELECT user_id, first_name, last_name, email, phone, user_type, created_at FROM Users", 
                    ConnectionString);
                SqlCommandBuilder usersCmdBuilder = new SqlCommandBuilder(usersAdapter);
                usersAdapter.Fill(ds, "Users");
                usersBindingSource.DataSource = ds;
                usersBindingSource.DataMember = "Users";

                // Load Properties
                propertiesAdapter = new SqlDataAdapter(
                    "SELECT property_id, host_id, title, description, address, city, state, country, " +
                    "zipcode, property_type, price_per_night, created_at FROM Properties", 
                    ConnectionString);
                SqlCommandBuilder propertiesCmdBuilder = new SqlCommandBuilder(propertiesAdapter);
                propertiesAdapter.Fill(ds, "Properties");

                // Set up relationship
                DataRelation relation = new DataRelation("UsersProperties",
                    ds.Tables["Users"].Columns["user_id"],
                    ds.Tables["Properties"].Columns["host_id"]);
                ds.Relations.Add(relation);

                propertiesBindingSource.DataSource = usersBindingSource;
                propertiesBindingSource.DataMember = "UsersProperties";
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error loading data: " + ex.Message);
            }
        }

        private void DataGridViewUsers_SelectionChanged(object sender, EventArgs e)
        {
            // Properties will automatically update due to binding
        }

        private void btnSaveChanges_Click(object sender, EventArgs e)
        {
            try
            {
                // Get changes for both tables
                DataTable usersChanges = ds.Tables["Users"].GetChanges();
                DataTable propertiesChanges = ds.Tables["Properties"].GetChanges();

                if (usersChanges != null || propertiesChanges != null)
                {
                    // Save changes to both tables
                    if (usersChanges != null)
                    {
                        usersAdapter.Update(usersChanges);
                    }
                    if (propertiesChanges != null)
                    {
                        propertiesAdapter.Update(propertiesChanges);
                    }

                    // Accept changes to update the DataSet state
                    ds.AcceptChanges();
                    
                    // Refresh the data
                    ds.Clear();
                    LoadData();
                    
                    MessageBox.Show("Changes saved successfully.");
                }
                else
                {
                    MessageBox.Show("No changes to save.");
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error saving changes: " + ex.Message);
            }
        }
    }
}
