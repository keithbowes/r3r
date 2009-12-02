class Subscriptions
{
  private:
    bool m_first_get;
    char * m_internal;
    char * m_saved;
  public:
    Subscriptions();
    ~Subscriptions();

    void Load();
    void Queue();

    void Add(char * sub);
    void Delete(char * sub);

    int IndexOf(char * sub);
    char * GetNext();
    char * GetAll();
};

Subscriptions * GetSubscriptionsObject();
