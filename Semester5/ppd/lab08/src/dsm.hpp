#pragma once

#include <mpi.h>
#include <functional>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <optional>

namespace dsm {

enum class MessageType : int {
    WRITE_REQUEST = 1,
    WRITE_COMMIT = 2,
    CAS_REQUEST = 3,
    CAS_COMMIT = 4,
    CAS_RESPONSE = 5
};

struct Message {
    int type;
    int var_id;
    int value;
    int new_value;
    int seq_num;
    int sender;
    int request_id;
};

struct Variable {
    int value = 0;
    std::set<int> subscribers;
    int coordinator = -1;
    int next_seq = 0;
    int last_applied_seq = -1;
    std::map<int, Message> pending_commits;
};

using ChangeCallback = std::function<void(int var_id, int old_value, int new_value)>;

class DSM {
public:
    DSM(MPI_Comm comm, int num_variables);
    ~DSM();

    void subscribe(int var_id, const std::set<int>& subscribers);
    void set_callback(ChangeCallback cb);

    int read(int var_id) const;
    void write(int var_id, int value);
    bool compare_exchange(int var_id, int expected, int new_value);

    void sync();
    void close();

private:
    void send_message(int dest, const Message& msg);
    void broadcast_to_subscribers(int var_id, const Message& msg);
    void process_messages();
    void handle_message(const Message& msg, int source);
    void apply_commit(int var_id, const Message& msg);
    void try_apply_pending(int var_id);
    bool is_subscribed(int var_id) const;
    int get_coordinator(int var_id) const;

    MPI_Comm comm_;
    int rank_;
    int size_;
    int num_variables_;
    std::vector<Variable> variables_;
    ChangeCallback callback_;
    bool closed_ = false;

    std::map<int, bool> pending_cas_result_;
    std::map<int, bool> cas_result_ready_;

    int next_request_id_ = 0;
    std::set<int> completed_requests_;
};

} // namespace dsm
