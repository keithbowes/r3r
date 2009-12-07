class Subscriptions
{
  private:
    unsigned int m_current;
  public:
    Subscriptions();

    void Add(char * sub);
    void Delete(char * sub);

    int IndexOf(char * sub);
    char * GetNext();
};

Subscriptions * GetSubscriptionsObject();
